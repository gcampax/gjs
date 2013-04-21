/* -*- mode: C; c-basic-offset: 4; indent-tabs-mode: nil; -*- */
/*
 * Copyright (c) 2008  litl, LLC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include <config.h>

#include "function.h"
#include "arg.h"
#include "object.h"
#include "boxed.h"
#include "union.h"
#include "gerror.h"
#include "closure.h"
#include "arg-cache.h"
#include <gjs/gjs-module.h>
#include <gjs/compat.h>

#include <util/log.h>

#include <girepository.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

/* We use guint8 for arguments; functions can't
 * have more than this.
 */
#define GJS_ARG_INDEX_INVALID G_MAXUINT8

typedef struct {
    GIFunctionInfo *info;

    GjsArgumentCache *arguments;

    guint8 js_in_argc;
    guint8 js_out_argc;
    GIFunctionInvoker invoker;
} Function;

static struct JSClass gjs_function_class;

/* Because we can't free the mmap'd data for a callback
 * while it's in use, this list keeps track of ones that
 * will be freed the next time we invoke a C function.
 */
static GSList *completed_trampolines = NULL;  /* GjsCallbackTrampoline */

GJS_DEFINE_PRIV_FROM_JS(Function, gjs_function_class)

void
gjs_callback_trampoline_ref(GjsCallbackTrampoline *trampoline)
{
    trampoline->ref_count++;
}

void
gjs_callback_trampoline_unref(GjsCallbackTrampoline *trampoline)
{
    /* Not MT-safe, like all the rest of GJS */

    trampoline->ref_count--;
    if (trampoline->ref_count == 0) {
        g_closure_unref(trampoline->js_function);
        g_callable_info_free_closure(trampoline->info, trampoline->closure);
        g_base_info_unref( (GIBaseInfo*) trampoline->info);
        g_free (trampoline->param_types);
        g_slice_free(GjsCallbackTrampoline, trampoline);
    }
}

static void
set_return_ffi_arg_from_giargument (GITypeInfo  *ret_type,
                                    void        *result,
                                    GIArgument  *return_value)
{
    switch (g_type_info_get_tag(ret_type)) {
    case GI_TYPE_TAG_INT8:
        *(ffi_sarg *) result = return_value->v_int8;
        break;
    case GI_TYPE_TAG_UINT8:
        *(ffi_arg *) result = return_value->v_uint8;
        break;
    case GI_TYPE_TAG_INT16:
        *(ffi_sarg *) result = return_value->v_int16;
        break;
    case GI_TYPE_TAG_UINT16:
        *(ffi_arg *) result = return_value->v_uint16;
        break;
    case GI_TYPE_TAG_INT32:
        *(ffi_sarg *) result = return_value->v_int32;
        break;
    case GI_TYPE_TAG_UINT32:
    case GI_TYPE_TAG_BOOLEAN:
    case GI_TYPE_TAG_UNICHAR:
        *(ffi_arg *) result = return_value->v_uint32;
		
        break;
    case GI_TYPE_TAG_INT64:
        *(ffi_sarg *) result = return_value->v_int64;
        break;
    case GI_TYPE_TAG_UINT64:
        *(ffi_arg *) result = return_value->v_uint64;
        break;
    case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo* interface_info;
            GIInfoType interface_type;

            interface_info = g_type_info_get_interface(ret_type);
            interface_type = g_base_info_get_type(interface_info);

            switch (interface_type) {
            case GI_INFO_TYPE_ENUM:
            case GI_INFO_TYPE_FLAGS:
                *(ffi_sarg *) result = return_value->v_long;
                break;
            default:
                *(ffi_arg *) result = (ffi_arg) return_value->v_pointer;
                break;
            }

            g_base_info_unref(interface_info);
        }
    default:
        *(ffi_arg *) result = (ffi_arg) return_value->v_uint64;
        break;
    }
}

/* This is our main entry point for ffi_closure callbacks.
 * ffi_prep_closure is doing pure magic and replaces the original
 * function call with this one which gives us the ffi arguments,
 * a place to store the return value and our use data.
 * In other words, everything we need to call the JS function and
 * getting the return value back.
 */
static void
gjs_callback_closure(ffi_cif *cif,
                     void *result,
                     void **args,
                     void *data)
{
    JSContext *context;
    GjsCallbackTrampoline *trampoline;
    int i, n_args, n_jsargs, n_outargs;
    jsval *jsargs, rval;
    JSObject *this_object;
    GITypeInfo ret_type;
    gboolean success = FALSE;
    gboolean ret_type_is_void;

    trampoline = data;
    g_assert(trampoline);
    gjs_callback_trampoline_ref(trampoline);

    context = gjs_closure_get_context(trampoline->js_function);
    JS_BeginRequest(context);

    n_args = g_callable_info_get_n_args(trampoline->info);

    g_assert(n_args >= 0);

    n_outargs = 0;
    jsargs = (jsval*)g_newa(jsval, n_args);
    for (i = 0, n_jsargs = 0; i < n_args; i++) {
        GIArgInfo arg_info;
        GITypeInfo type_info;
        GjsParamType param_type;

        g_callable_info_load_arg(trampoline->info, i, &arg_info);
        g_arg_info_load_type(&arg_info, &type_info);

        /* Skip void * arguments */
        if (g_type_info_get_tag(&type_info) == GI_TYPE_TAG_VOID)
            continue;

        if (g_arg_info_get_direction(&arg_info) == GI_DIRECTION_OUT) {
            n_outargs++;
            continue;
        }

        if (g_arg_info_get_direction(&arg_info) == GI_DIRECTION_INOUT)
            n_outargs++;

        param_type = trampoline->param_types[i];

        switch (param_type) {
            case PARAM_SKIPPED:
                continue;
            case PARAM_ARRAY: {
                gint array_length_pos = g_type_info_get_array_length(&type_info);
                GIArgInfo array_length_arg;
                GITypeInfo arg_type_info;
                jsval length;

                g_callable_info_load_arg(trampoline->info, array_length_pos, &array_length_arg);
                g_arg_info_load_type(&array_length_arg, &arg_type_info);
                if (!gjs_value_from_g_argument(context, &length,
                                               &arg_type_info,
                                               args[array_length_pos], TRUE))
                    goto out;

                if (!gjs_value_from_explicit_array(context, &jsargs[n_jsargs++],
                                                   &type_info, args[i], JSVAL_TO_INT(length)))
                    goto out;
                break;
            }
            case PARAM_NORMAL:
                if (!gjs_value_from_g_argument(context,
                                               &jsargs[n_jsargs++],
                                               &type_info,
                                               args[i], FALSE))
                    goto out;
                break;
            default:
                g_assert_not_reached();
        }
    }

    if (trampoline->is_vfunc) {
        g_assert(n_args > 0);
        this_object = JSVAL_TO_OBJECT(jsargs[0]);
        jsargs++;
        n_jsargs--;
    } else {
        this_object = NULL;
    }

    if (!gjs_closure_invoke(trampoline->js_function,
                            this_object,
                            n_jsargs,
                            jsargs,
                            &rval)) {
        goto out;
    }

    g_callable_info_load_return_type(trampoline->info, &ret_type);
    ret_type_is_void = g_type_info_get_tag (&ret_type) == GI_TYPE_TAG_VOID;

    if (n_outargs == 0 && !ret_type_is_void) {
        GIArgument argument;

        /* non-void return value, no out args. Should
         * be a single return value. */
        if (!gjs_value_to_g_argument(context,
                                     rval,
                                     &ret_type,
                                     "callback",
                                     GJS_ARGUMENT_RETURN_VALUE,
                                     GI_TRANSFER_NOTHING,
                                     TRUE,
                                     &argument))
            goto out;

        set_return_ffi_arg_from_giargument(&ret_type,
                                           result,
                                           &argument);
    } else if (n_outargs == 1 && ret_type_is_void) {
        /* void return value, one out args. Should
         * be a single return value. */
        for (i = 0; i < n_args; i++) {
            GIArgInfo arg_info;
            GITypeInfo type_info;
            g_callable_info_load_arg(trampoline->info, i, &arg_info);
            if (g_arg_info_get_direction(&arg_info) == GI_DIRECTION_IN)
                continue;

            g_arg_info_load_type(&arg_info, &type_info);
            if (!gjs_value_to_g_argument(context,
                                         rval,
                                         &type_info,
                                         "callback",
                                         GJS_ARGUMENT_ARGUMENT,
                                         GI_TRANSFER_NOTHING,
                                         TRUE,
                                         *(gpointer *)args[i]))
                goto out;

            break;
        }
    } else {
        jsval elem;
        gsize elem_idx = 0;
        /* more than one of a return value or an out argument.
         * Should be an array of output values. */

        if (!ret_type_is_void) {
            GIArgument argument;

            if (!JS_GetElement(context, JSVAL_TO_OBJECT(rval), elem_idx, &elem))
                goto out;

            if (!gjs_value_to_g_argument(context,
                                         elem,
                                         &ret_type,
                                         "callback",
                                         GJS_ARGUMENT_ARGUMENT,
                                         GI_TRANSFER_NOTHING,
                                         TRUE,
                                         &argument))
                goto out;

            set_return_ffi_arg_from_giargument(&ret_type,
                                               result,
                                               &argument);

            elem_idx++;
        }

        for (i = 0; i < n_args; i++) {
            GIArgInfo arg_info;
            GITypeInfo type_info;
            g_callable_info_load_arg(trampoline->info, i, &arg_info);
            if (g_arg_info_get_direction(&arg_info) == GI_DIRECTION_IN)
                continue;

            g_arg_info_load_type(&arg_info, &type_info);
            if (!JS_GetElement(context, JSVAL_TO_OBJECT(rval), elem_idx, &elem))
                goto out;

            if (!gjs_value_to_g_argument(context,
                                         elem,
                                         &type_info,
                                         "callback",
                                         GJS_ARGUMENT_ARGUMENT,
                                         GI_TRANSFER_NOTHING,
                                         TRUE,
                                         *(gpointer *)args[i]))
                goto out;

            elem_idx++;
        }
    }

    success = TRUE;

out:
    if (!success) {
        /* Fill in the result with some hopefully neutral value */
        g_callable_info_load_return_type(trampoline->info, &ret_type);
        gjs_g_argument_init_default (context, &ret_type, result);
    }

    if (trampoline->scope == GI_SCOPE_TYPE_ASYNC) {
        completed_trampolines = g_slist_prepend(completed_trampolines, trampoline);
    }

    gjs_callback_trampoline_unref(trampoline);
    JS_EndRequest(context);
}

GjsCallbackTrampoline*
gjs_callback_trampoline_new(JSContext      *context,
                            jsval           function,
                            GICallableInfo *callable_info,
                            GIScopeType     scope,
                            JSObject       *scope_object,
                            gboolean        is_vfunc)
{
    GjsCallbackTrampoline *trampoline;
    int n_args, i;
    gboolean should_root;

    if (JSVAL_IS_NULL(function)) {
        return NULL;
    }

    g_assert(JS_TypeOfValue(context, function) == JSTYPE_FUNCTION);

    trampoline = g_slice_new(GjsCallbackTrampoline);
    trampoline->ref_count = 1;
    trampoline->info = callable_info;
    g_base_info_ref((GIBaseInfo*)trampoline->info);

    /* The rule is:
     * - async and call callbacks are rooted
     * - callbacks in GObjects methods are traced from the GObject
     *   (and same for vfuncs, which are associated with a GObject prototype)
     */
    should_root = scope != GI_SCOPE_TYPE_NOTIFIED || scope_object == NULL;
    trampoline->js_function = gjs_closure_new(context,
                                              JSVAL_TO_OBJECT(function),
                                              g_base_info_get_name((GIBaseInfo*)callable_info),
                                              should_root);
    if (!should_root && scope_object)
        gjs_object_associate_closure(context, scope_object, trampoline->js_function);

    /* Analyze param types and directions, similarly to init_cached_function_data */
    n_args = g_callable_info_get_n_args(trampoline->info);
    trampoline->param_types = g_new0(GjsParamType, n_args);

    for (i = 0; i < n_args; i++) {
        GIDirection direction;
        GIArgInfo arg_info;
        GITypeInfo type_info;
        GITypeTag type_tag;

        if (trampoline->param_types[i] == PARAM_SKIPPED)
            continue;

        g_callable_info_load_arg(trampoline->info, i, &arg_info);
        g_arg_info_load_type(&arg_info, &type_info);

        direction = g_arg_info_get_direction(&arg_info);
        type_tag = g_type_info_get_tag(&type_info);

        if (direction != GI_DIRECTION_IN) {
            /* INOUT and OUT arguments are handled differently. */
            continue;
        }

        if (type_tag == GI_TYPE_TAG_INTERFACE) {
            GIBaseInfo* interface_info;
            GIInfoType interface_type;

            interface_info = g_type_info_get_interface(&type_info);
            interface_type = g_base_info_get_type(interface_info);
            if (interface_type == GI_INFO_TYPE_CALLBACK) {
                gjs_throw(context, "Callback accepts another callback as a parameter. This is not supported");
                g_base_info_unref(interface_info);
                return NULL;
            }
            g_base_info_unref(interface_info);
        } else if (type_tag == GI_TYPE_TAG_ARRAY) {
            if (g_type_info_get_array_type(&type_info) == GI_ARRAY_TYPE_C) {
                int array_length_pos = g_type_info_get_array_length(&type_info);

                if (array_length_pos >= 0 && array_length_pos < n_args) {
                    GIArgInfo length_arg_info;

                    g_callable_info_load_arg(trampoline->info, array_length_pos, &length_arg_info);
                    if (g_arg_info_get_direction(&length_arg_info) != direction) {
                        gjs_throw(context, "Callback has an array with different-direction length arg, not supported");
                        return NULL;
                    }

                    trampoline->param_types[array_length_pos] = PARAM_SKIPPED;
                    trampoline->param_types[i] = PARAM_ARRAY;
                }
            }
        }
    }

    trampoline->closure = g_callable_info_prepare_closure(callable_info, &trampoline->cif,
                                                          gjs_callback_closure, trampoline);

    trampoline->scope = scope;
    trampoline->is_vfunc = is_vfunc;

    return trampoline;
}

static JSBool
gjs_fill_method_instance (JSContext  *context,
                          JSObject   *obj,
                          Function   *function,
                          GIArgument *out_arg,
                          gboolean   *is_gobject)
{
    GIBaseInfo *container = g_base_info_get_container((GIBaseInfo *) function->info);
    GIInfoType type = g_base_info_get_type(container);
    GType gtype = g_registered_type_info_get_g_type ((GIRegisteredTypeInfo *)container);

    *is_gobject = FALSE;

    switch (type) {
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
        /* GError must be special cased */
        if (g_type_is_a(gtype, G_TYPE_ERROR)) {
            if (!gjs_typecheck_gerror(context, obj, JS_TRUE))
                return JS_FALSE;

            out_arg->v_pointer = gjs_gerror_from_error(context, obj);
        } else {
            if (!gjs_typecheck_boxed(context, obj,
                                     container, gtype,
                                     JS_TRUE))
                return JS_FALSE;

            out_arg->v_pointer = gjs_c_struct_from_boxed(context, obj);
        }
        break;

    case GI_INFO_TYPE_UNION:
        if (!gjs_typecheck_union(context, obj,
                                 container, gtype, JS_TRUE))
            return JS_FALSE;

        out_arg->v_pointer = gjs_c_union_from_union(context, obj);
        break;

    case GI_INFO_TYPE_OBJECT:
    case GI_INFO_TYPE_INTERFACE:
        if (!gjs_typecheck_object(context, obj,
                                  gtype, JS_TRUE))
            return JS_FALSE;

        out_arg->v_pointer = gjs_g_object_from_object(context, obj);
        *is_gobject = TRUE;
        break;

    default:
        g_assert_not_reached();
    }

    return JS_TRUE;
}

static void
complete_async_calls(void)
{
    GSList *iter;

    if (completed_trampolines) {
        for (iter = completed_trampolines; iter; iter = iter->next) {
            GjsCallbackTrampoline *trampoline = iter->data;
            gjs_callback_trampoline_unref(trampoline);
        }
        g_slist_free(completed_trampolines);
        completed_trampolines = NULL;
    }
}

static JSBool
gjs_invoke_c_function(JSContext      *context,
                      Function       *function,
                      JSObject       *obj, /* "this" object */
                      unsigned        js_argc,
                      jsval          *js_argv,
                      jsval          *js_rval)
{
    GjsFunctionCallState state;
    gpointer *ffi_arg_pointers;
    GIFFIReturnValue return_value;
    gpointer return_value_p; /* Will point inside the union return_value */

    int processed_c_args = 0;
    int gi_argc, gi_arg_pos;
    int ffi_argc, ffi_arg_pos;
    int js_arg_pos;
    gboolean can_throw_gerror;
    gboolean did_throw_gerror = FALSE;
    GError *local_error = NULL, **errorp;
    gboolean failed, postinvoke_release_failed;

    gboolean is_method;
    gboolean is_object_method = FALSE;
    GITypeTag return_tag;
    jsval *return_values = NULL;

    /* Because we can't free a closure while we're in it, we defer
     * freeing until the next time a C function is invoked.  What
     * we should really do instead is queue it for a GC thread.
     */
    complete_async_calls();

    is_method = g_callable_info_is_method(function->info);
    can_throw_gerror = g_callable_info_can_throw_gerror(function->info);

    /* @c_argc is the number of arguments that the underlying C
     * function takes. @gi_argc is the number of arguments the
     * GICallableInfo describes (which does not include "this" or
     * GError**). @function->js_in_argc is the number of
     * arguments we expect the JS function to take (which does not
     * include PARAM_SKIPPED args).
     *
     * @js_argc is the number of arguments that were actually passed;
     * we allow this to be larger than @js_in_argc for
     * convenience, and simply ignore the extra arguments. But we
     * don't allow too few args, since that would break.
     */

    if (js_argc < function->js_in_argc) {
        gjs_throw(context, "Too few arguments to %s %s.%s expected %d got %d",
                  is_method ? "method" : "function",
                  g_base_info_get_namespace( (GIBaseInfo*) function->info),
                  g_base_info_get_name( (GIBaseInfo*) function->info),
                  function->js_in_argc,
                  js_argc);
        return JS_FALSE;
    }

    /* These first four are arrays which hold argument pointers.
     * @in_arg_cvalues: C values which are passed on input (in or inout)
     * @out_arg_cvalues: C values which are returned as arguments (out or inout)
     * @inout_original_arg_cvalues: For the special case of (inout) args, we need to
     *  keep track of the original values we passed into the function, in case we
     *  need to free it.
     * @ffi_arg_pointers: For passing data to FFI, we need to create another layer
     *  of indirection; this array is a pointer to an element in in_arg_cvalues
     *  or out_arg_cvalues.
     * @return_value: The actual return value of the C function, i.e. not an (out) param
     *
     * The 3 GArgument arrays are indexed by the GI argument index,
     * with the following exceptions:
     * [-1] is the return value (which can be nothing/garbage if the
     * function returns void)
     * [-2] is the instance parameter, if present
     * ffi_arg_pointers, on the other hand, represents the actual
     * C arguments, in the way ffi expects them
     *
     * Use gi_arg_pos to index inside the GArgument array
     * Use ffi_arg_pos to index inside ffi_arg_pointers
    */

    ffi_argc = function->invoker.cif.nargs;
    gi_argc = g_callable_info_get_n_args( (GICallableInfo*) function->info);

    if (is_method) {
        state.in_arg_cvalues = g_newa(GArgument, gi_argc + 2) + 2;
        state.out_arg_cvalues = g_newa(GArgument, gi_argc + 2) + 2;
        state.inout_original_arg_cvalues = g_newa(GArgument, gi_argc + 2) + 2;
    } else {
        state.in_arg_cvalues = g_newa(GArgument, gi_argc + 1) + 1;
        state.out_arg_cvalues = g_newa(GArgument, gi_argc + 1) + 1;
        state.inout_original_arg_cvalues = g_newa(GArgument, gi_argc + 1) + 1;
    }

    ffi_arg_pointers = g_newa(gpointer, ffi_argc);
    state.argv = js_argv;

    failed = FALSE;
    ffi_arg_pos = 0; /* index into ffi_arg_pointers */
    js_arg_pos = 0; /* index into argv */

    if (is_method) {
        if (!gjs_fill_method_instance(context, obj,
                                      function, &state.in_arg_cvalues[-2],
                                      &is_object_method))
            return JS_FALSE;
        ffi_arg_pointers[0] = &state.in_arg_cvalues[-2];
        ++ffi_arg_pos;
    }

    processed_c_args = ffi_arg_pos;
    for (gi_arg_pos = 0; gi_arg_pos < gi_argc; gi_arg_pos++, ffi_arg_pos++) {
        GjsArgumentCache *cache;
        GArgument *in_value;

        cache = &function->arguments[gi_arg_pos];
        in_value = &state.in_arg_cvalues[gi_arg_pos];
        ffi_arg_pointers[ffi_arg_pos] = in_value;

        if (!cache->marshal_in(context, cache,
                               &state, in_value,
                               state.argv[js_arg_pos])) {
            failed = TRUE;
            break;
        }

        if (!gjs_arg_cache_is_skip_in(cache))
            js_arg_pos++;

        processed_c_args++;
    }

    /* Did argument conversion fail?  In that case, skip invocation and jump to release
     * processing. */
    if (failed) {
        did_throw_gerror = FALSE;
        goto release;
    }

    if (can_throw_gerror) {
        errorp = &local_error;
        ffi_arg_pointers[ffi_arg_pos] = &errorp;
        ffi_arg_pos++;

        /* don't update processed_c_args as we deal with local_error
         * separately */
    }

    g_assert_cmpuint(ffi_arg_pos, ==, ffi_argc);
    g_assert_cmpuint(gi_arg_pos, ==, gi_argc);

    if (!gjs_arg_cache_is_skip_out(&function->arguments[-1])) {
        return_tag = g_type_info_get_tag(&function->arguments[-1].type_info);

        /* See comment for GjsFFIReturnValue above */
        if (return_tag == GI_TYPE_TAG_FLOAT)
            return_value_p = &return_value.v_float;
        else if (return_tag == GI_TYPE_TAG_DOUBLE)
            return_value_p = &return_value.v_double;
        else if (return_tag == GI_TYPE_TAG_INT64 || return_tag == GI_TYPE_TAG_UINT64)
            return_value_p = &return_value.v_uint64;
        else
            return_value_p = &return_value.v_long;
    } else {
        return_value_p = NULL;
    }

    ffi_call(&(function->invoker.cif), function->invoker.native_address, return_value_p, ffi_arg_pointers);

    /* Return value and out arguments are valid only if invocation doesn't
     * return error. In arguments need to be released always.
     */
    if (can_throw_gerror) {
        did_throw_gerror = local_error != NULL;
    } else {
        did_throw_gerror = FALSE;
    }

    *js_rval = JSVAL_VOID;

    if (!gjs_arg_cache_is_skip_out(&function->arguments[-1])) {
        gi_type_info_extract_ffi_return_value(&function->arguments[-1].type_info,
                                              &return_value,
                                              &state.out_arg_cvalues[-1]);
    }

    if (function->js_out_argc > 0) {
        return_values = g_newa(jsval, function->js_out_argc);
        gjs_set_values(context, return_values, function->js_out_argc, JSVAL_VOID);
        gjs_root_value_locations(context, return_values, function->js_out_argc);
    }

    /* Process out arguments and return values
       This loop is skipped if we fail the type conversion above, or
       if did_throw_gerror is true.
    */
    js_arg_pos = 0;
    for (gi_arg_pos = -1; gi_arg_pos < gi_argc; gi_arg_pos++) {
        GjsArgumentCache *cache;
        GArgument *out_value;

        cache = &function->arguments[gi_arg_pos];
        out_value = &state.out_arg_cvalues[gi_arg_pos];

        if (!cache->marshal_out(context, cache,
                                &state, out_value,
                                &return_values[js_arg_pos])) {
            failed = TRUE;
            break;
        }

        if (!gjs_arg_cache_is_skip_out(cache))
            js_arg_pos++;
    }

    g_assert(failed || did_throw_gerror || js_arg_pos == (guint8)function->js_out_argc);

 release:
    /* In this loop we use ffi_arg_pos just to ensure we don't release stuff
       we haven't allocated yet, if we failed in type conversion above.
       Because we start from -1 (the return value), we need to process 1 more than processed_c_args
    */
    ffi_arg_pos = is_method ? 1 : 0;
    postinvoke_release_failed = FALSE;
    for (gi_arg_pos = -1; gi_arg_pos < gi_argc && ffi_arg_pos < (processed_c_args + 1); gi_arg_pos++, ffi_arg_pos++) {
        GjsArgumentCache *cache;
        GArgument *in_value, *out_value;

        cache = &function->arguments[gi_arg_pos];
        in_value = &state.in_arg_cvalues[gi_arg_pos];
        out_value = &state.out_arg_cvalues[gi_arg_pos];

        /* Only process in or inout arguments if we failed, the rest is garbage */
        if (failed && gjs_arg_cache_is_skip_in(cache))
            continue;

        if (!cache->release(context, cache,
                            &state, in_value, out_value)) {
            postinvoke_release_failed = TRUE;
            /* continue with the release even if we fail, to avoid leaks */
        }
    }

    if (postinvoke_release_failed)
        failed = TRUE;

    g_assert_cmpuint(ffi_arg_pos, ==, processed_c_args + 1);

    if (function->js_out_argc > 0 && (!failed && !did_throw_gerror)) {
        /* if we have 1 return value or out arg, return that item
         * on its own, otherwise return a JavaScript array with
         * [return value, out arg 1, out arg 2, ...]
         */
        if (function->js_out_argc == 1) {
            *js_rval = return_values[0];
        } else {
            JSObject *array;
            array = JS_NewArrayObject(context,
                                      function->js_out_argc,
                                      return_values);
            if (array == NULL) {
                failed = TRUE;
            } else {
                *js_rval = OBJECT_TO_JSVAL(array);
            }
        }

        gjs_unroot_value_locations(context, return_values, function->js_out_argc);
    }

    if (!failed && did_throw_gerror) {
        gjs_throw_g_error(context, local_error);
        return JS_FALSE;
    } else if (failed) {
        return JS_FALSE;
    } else {
        return JS_TRUE;
    }
}

static JSBool
function_call(JSContext *context,
              unsigned   js_argc,
              jsval     *vp)
{
    jsval *js_argv = JS_ARGV(context, vp);
    JSObject *object = JS_THIS_OBJECT(context, vp);
    JSObject *callee = JSVAL_TO_OBJECT(JS_CALLEE(context, vp));
    JSBool success;
    Function *priv;
    jsval retval;

    priv = priv_from_js(context, callee);
    gjs_debug_marshal(GJS_DEBUG_GFUNCTION, "Call callee %p priv %p this obj %p %s", callee, priv,
                      obj, JS_GetTypeName(context,
                                          JS_TypeOfValue(context, OBJECT_TO_JSVAL(object))));

    if (priv == NULL)
        return JS_TRUE; /* we are the prototype, or have the wrong class */


    success = gjs_invoke_c_function(context, priv, object, js_argc, js_argv, &retval);
    if (success)
        JS_SET_RVAL(context, vp, retval);

    return success;
}

GJS_NATIVE_CONSTRUCTOR_DEFINE_ABSTRACT(function)

/* Does not actually free storage for structure, just
 * reverses init_cached_function_data
 */
static void
uninit_cached_function_data (Function *function)
{
    if (function->info)
        g_base_info_unref( (GIBaseInfo*) function->info);
    function->info = NULL;

    /* Careful! function->arguments is one inside an array */
    if (function->arguments)
        g_free(&function->arguments[-1]);
    function->arguments = NULL;

    g_function_invoker_destroy(&function->invoker);
}

static void
function_finalize(JSContext *context,
                  JSObject  *obj)
{
    Function *priv;

    priv = priv_from_js(context, obj);
    gjs_debug_lifecycle(GJS_DEBUG_GFUNCTION,
                        "finalize, obj %p priv %p", obj, priv);
    if (priv == NULL)
        return; /* we are the prototype, not a real instance, so constructor never called */

    uninit_cached_function_data(priv);

    GJS_DEC_COUNTER(function);
    g_slice_free(Function, priv);
}

static JSBool
get_num_arguments (JSContext *context,
                   JSObject **obj,
                   jsid      *id,
                   jsval     *vp)
{
    jsval retval;
    Function *priv;

    priv = priv_from_js(context, *obj);

    retval = INT_TO_JSVAL(priv->js_in_argc);
    JS_SET_RVAL(context, vp, retval);
    return JS_TRUE;
}

static JSBool
function_to_string (JSContext *context,
                    guint      argc,
                    jsval     *vp)
{
    Function *priv;
    gchar *string;
    gboolean free;
    JSObject *self;
    jsval retval;
    JSBool ret = JS_FALSE;
    int i, n_args, n_jsargs;
    GString *arg_names_str;
    gchar *arg_names;

    self = JS_THIS_OBJECT(context, vp);
    if (!self) {
        gjs_throw(context, "this cannot be null");
        return JS_FALSE;
    }

    priv = priv_from_js (context, self);
    if (priv == NULL) {
        string = "function () {\n}";
        free = FALSE;
        goto out;
    }

    free = TRUE;

    n_args = g_callable_info_get_n_args(priv->info);
    n_jsargs = 0;
    arg_names_str = g_string_new("");
    for (i = 0; i < n_args; i++) {
        GIArgInfo arg_info;

        if (gjs_arg_cache_is_skip_in(&priv->arguments[i]))
            continue;

        g_callable_info_load_arg(priv->info, i, &arg_info);

        if (g_arg_info_get_direction(&arg_info) == GI_DIRECTION_OUT)
            continue;

        if (n_jsargs > 0)
            g_string_append(arg_names_str, ", ");

        n_jsargs++;
        g_string_append(arg_names_str, g_base_info_get_name(&arg_info));
    }
    arg_names = g_string_free(arg_names_str, FALSE);

    if (g_base_info_get_type(priv->info) == GI_INFO_TYPE_FUNCTION) {
        string = g_strdup_printf("function %s(%s) {\n\t/* proxy for native symbol %s(); */\n}",
                                 g_base_info_get_name ((GIBaseInfo *) priv->info),
                                 arg_names,
                                 g_function_info_get_symbol ((GIFunctionInfo *) priv->info));
    } else {
        string = g_strdup_printf("function %s(%s) {\n\t/* proxy for native symbol */\n}",
                                 g_base_info_get_name ((GIBaseInfo *) priv->info),
                                 arg_names);
    }

    g_free(arg_names);

 out:
    if (gjs_string_from_utf8(context, string, -1, &retval)) {
        JS_SET_RVAL(context, vp, retval);
        ret = JS_TRUE;
    }

    if (free)
        g_free(string);
    return ret;
}

/* The bizarre thing about this vtable is that it applies to both
 * instances of the object, and to the prototype that instances of the
 * class have.
 */
static struct JSClass gjs_function_class = {
    "GIRepositoryFunction", /* means "new GIRepositoryFunction()" works */
    JSCLASS_HAS_PRIVATE,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_StrictPropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    function_finalize,
    NULL,
    function_call,
    NULL, NULL, NULL
};

static JSPropertySpec gjs_function_proto_props[] = {
    { "length", 0,
      (JSPROP_READONLY | JSPROP_PERMANENT | JSPROP_SHARED),
      JSOP_WRAPPER((JSPropertyOp)get_num_arguments),
      JSOP_WRAPPER(JS_StrictPropertyStub)
    },
    { NULL }
};

/* The original Function.prototype.toString complains when
   given a GIRepository function as an argument */
static JSFunctionSpec gjs_function_proto_funcs[] = {
    JS_FN("toString", function_to_string, 0, 0),
    JS_FS_END
};

static void
throw_not_introspectable_argument(JSContext      *context,
                                  GICallableInfo *function,
                                  GIArgInfo      *arg)
{
    gjs_throw(context, "Function %s.%s cannot be called: argument '%s' is not introspectable.",
              g_base_info_get_namespace((GIBaseInfo*) function),
              g_base_info_get_name((GIBaseInfo*) function),
              g_base_info_get_name((GIBaseInfo*) arg));
}

static gboolean
init_cached_function_data (JSContext      *context,
                           Function       *function,
                           GType           gtype,
                           GICallableInfo *info)
{
    guint8 i, n_args;
    GError *error = NULL;
    GIInfoType info_type;
    GjsArgumentCache *arguments;
    int in_argc, out_argc;
    JSBool inc_counter;

    info_type = g_base_info_get_type((GIBaseInfo *)info);

    if (info_type == GI_INFO_TYPE_FUNCTION) {
        if (!g_function_info_prep_invoker((GIFunctionInfo *)info,
                                          &(function->invoker),
                                          &error)) {
            gjs_throw_g_error(context, error);
            return FALSE;
        }
    } else if (info_type == GI_INFO_TYPE_VFUNC) {
        gpointer addr;

        addr = g_vfunc_info_get_address((GIVFuncInfo *)info, gtype, &error);
        if (error != NULL) {
            if (error->code != G_INVOKE_ERROR_SYMBOL_NOT_FOUND)
                gjs_throw_g_error(context, error);

            g_clear_error(&error);
            return FALSE;
        }

        if (!g_function_invoker_new_for_address(addr, info,
                                                &(function->invoker),
                                                &error)) {
            gjs_throw_g_error(context, error);
            return FALSE;
        }
    }

    n_args = g_callable_info_get_n_args(info);

    /* arguments is one inside an array of n_args + 1, so
       arguments[-1] is the return value (if any)
    */
    arguments = g_new0(GjsArgumentCache, n_args + 1) + 1;

    if (!gjs_arg_cache_build_return(&arguments[-1],
                                    arguments,
                                    info, &inc_counter)) {
        gjs_throw(context, "Function %s.%s cannot be called: the return value is not introspectable.",
                  g_base_info_get_namespace((GIBaseInfo*) info),
                  g_base_info_get_name((GIBaseInfo*) info));
        return FALSE;
    }
    out_argc = inc_counter ? 1 : 0;
    in_argc = 0;

    for (i = 0; i < n_args; i++) {
        GIDirection direction;
        GIArgInfo arg_info;

        if (gjs_arg_cache_is_skip_in(&arguments[i]) ||
            gjs_arg_cache_is_skip_out(&arguments[i]))
            continue;

        g_callable_info_load_arg((GICallableInfo*) info, i, &arg_info);
        direction = g_arg_info_get_direction(&arg_info);

        if (!gjs_arg_cache_build_arg(&arguments[i],
                                     arguments, i,
                                     direction, &arg_info, info,
                                     &inc_counter)) {
            throw_not_introspectable_argument(context, info, &arg_info);
            return FALSE;
        }

        if (inc_counter) {
            if (direction == GI_DIRECTION_IN) {
                in_argc++;
            } else if (direction == GI_DIRECTION_INOUT) {
                in_argc++;
                out_argc++;
            } else { /* GI_DIRECTION_OUT */
                out_argc++;
            }
        }
    }

    function->arguments = arguments;

    function->js_in_argc = in_argc;
    function->js_out_argc = out_argc;
    function->info = info;

    g_base_info_ref((GIBaseInfo*) function->info);

    return JS_TRUE;
}

static JSObject*
function_new(JSContext      *context,
             GType           gtype,
             GICallableInfo *info)
{
    JSObject *function;
    JSObject *global;
    Function *priv;
    JSBool found;

    /* put constructor for GIRepositoryFunction() in the global namespace */
    global = gjs_get_import_global(context);

    if (!JS_HasProperty(context, global, gjs_function_class.name, &found))
        return NULL;
    if (!found) {
        JSObject *prototype;
        JSObject *parent_proto;
        jsval native_function;

        JS_GetProperty(context, global, "Function", &native_function);
        /* We take advantage from that fact that Function.__proto__ is Function.prototype */
        parent_proto = JS_GetPrototype(JSVAL_TO_OBJECT(native_function));

        prototype = JS_InitClass(context, global,
                                 /* parent prototype JSObject* for
                                  * prototype; NULL for
                                  * Object.prototype
                                  */
                                 parent_proto,
                                 &gjs_function_class,
                                 /* constructor for instances (NULL for
                                  * none - just name the prototype like
                                  * Math - rarely correct)
                                  */
                                 gjs_function_constructor,
                                 /* number of constructor args */
                                 0,
                                 /* props of prototype */
                                 &gjs_function_proto_props[0],
                                 /* funcs of prototype */
                                 &gjs_function_proto_funcs[0],
                                 /* props of constructor, MyConstructor.myprop */
                                 NULL,
                                 /* funcs of constructor, MyConstructor.myfunc() */
                                 NULL);
        if (prototype == NULL)
            gjs_fatal("Can't init class %s", gjs_function_class.name);

        gjs_debug(GJS_DEBUG_GFUNCTION, "Initialized class %s prototype %p",
                  gjs_function_class.name, prototype);
    }

    function = JS_NewObject(context, &gjs_function_class, NULL, global);
    if (function == NULL) {
        gjs_debug(GJS_DEBUG_GFUNCTION, "Failed to construct function");
        return NULL;
    }


    priv = g_slice_new0(Function);

    GJS_INC_COUNTER(function);

    g_assert(priv_from_js(context, function) == NULL);
    JS_SetPrivate(function, priv);

    gjs_debug_lifecycle(GJS_DEBUG_GFUNCTION,
                        "function constructor, obj %p priv %p", function, priv);

    if (!init_cached_function_data(context, priv, gtype, (GICallableInfo *)info))
      return NULL;

    return function;
}

JSObject*
gjs_define_function(JSContext      *context,
                    JSObject       *in_object,
                    GType           gtype,
                    GICallableInfo *info)
{
    JSObject *function = NULL;
    GIInfoType info_type;
    gchar *name;
    gboolean free_name;

    info_type = g_base_info_get_type((GIBaseInfo *)info);

    JS_BeginRequest(context);

    function = function_new(context, gtype, info);
    if (function == NULL) {
        gjs_move_exception(context, context);

        JS_EndRequest(context);
        return NULL;
    }

    if (info_type == GI_INFO_TYPE_FUNCTION) {
        name = (gchar *) g_base_info_get_name((GIBaseInfo*) info);
        free_name = FALSE;
    } else if (info_type == GI_INFO_TYPE_VFUNC) {
        name = g_strdup_printf("vfunc_%s", g_base_info_get_name((GIBaseInfo*) info));
        free_name = TRUE;
    } else {
        g_assert_not_reached ();
    }

    if (!JS_DefineProperty(context, in_object, name,
                           OBJECT_TO_JSVAL(function),
                           NULL, NULL,
                           GJS_MODULE_PROP_FLAGS)) {
        gjs_debug(GJS_DEBUG_GFUNCTION, "Failed to define function");

        JS_EndRequest(context);
        return NULL;
    }

    if (free_name)
        g_free(name);

    JS_EndRequest(context);
    return function;
}


JSBool
gjs_invoke_c_function_uncached (JSContext      *context,
                                GIFunctionInfo *info,
                                JSObject       *obj,
                                unsigned        argc,
                                jsval          *argv,
                                jsval          *rval)
{
  Function function;
  JSBool result;

  memset (&function, 0, sizeof (Function));
  if (!init_cached_function_data (context, &function, 0, info))
    return JS_FALSE;

  result = gjs_invoke_c_function (context, &function, obj, argc, argv, rval);
  uninit_cached_function_data (&function);
  return result;
}
