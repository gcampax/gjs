/* -*- mode: C; c-basic-offset: 4; indent-tabs-mode: nil; -*- */
/*
 * Copyright (c) 2013 Giovanni Campagna <scampa.giovanni@gmail.com>
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
#include "gtype.h"
#include <gjs/gjs-module.h>
#include <gjs/compat.h>

#include <util/log.h>

#include <girepository.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

static JSBool gjs_arg_cache_build_normal_in_arg (GjsArgumentCache *self,
                                                 GITypeTag         tag);

/* The global entry point for any invocations of GDestroyNotify;
 * look up the callback through the user_data and then free it.
 */
static void
gjs_destroy_notify_callback(gpointer data)
{
    GjsCallbackTrampoline *trampoline = data;

    g_assert(trampoline);
    gjs_callback_trampoline_unref(trampoline);
}

static unsigned long
gjs_g_argument_get_ulong(GITypeTag  tag,
                         GArgument *arg)
{
    switch(tag) {
    case GI_TYPE_TAG_INT8:
        return arg->v_int8;
    case GI_TYPE_TAG_UINT8:
        return arg->v_uint8;
    case GI_TYPE_TAG_INT16:
        return arg->v_int16;
    case GI_TYPE_TAG_UINT16:
        return arg->v_uint16;
    case GI_TYPE_TAG_INT32:
        return arg->v_int32;
    case GI_TYPE_TAG_UINT32:
        return arg->v_uint32;
    case GI_TYPE_TAG_INT64:
        return arg->v_int64;
    case GI_TYPE_TAG_UINT64:
        return arg->v_uint64;
    default:
        g_assert_not_reached ();
    }
}

static void
gjs_g_argument_set_ulong(GITypeTag      tag,
                         GArgument     *arg,
                         unsigned long  value)
{
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
    /* In a little endian system, the first byte
       of an unsigned long value is the same value,
       downcasted to uint8, and no code is needed.
       Also, we ignore the sign, as we're just moving
       bits here.
    */
    arg->v_ulong = value;
#else
    switch(tag) {
    case GI_TYPE_TAG_INT8:
        arg->v_int8 = value;
        break;
    case GI_TYPE_TAG_UINT8:
        arg->v_uint8 = value;
        break;
    case GI_TYPE_TAG_INT16:
        arg->v_int16 = value;
        break;
    case GI_TYPE_TAG_UINT16:
        arg->v_uint16 = value;
        break;
    case GI_TYPE_TAG_INT32:
        arg->v_int32 = value;
        break;
    case GI_TYPE_TAG_UINT32:
        arg->v_uint32 = value;
        break;
    case GI_TYPE_TAG_INT64:
        arg->v_int64 = value;
        break;
    case GI_TYPE_TAG_UINT64:
        arg->v_uint64 = value;
        break;
    }
#endif
}

/*
 * Marshallers:
 *
 * Each argument, irrespective of the direction, is processed
 * in three phases:
 * - before calling the C function [in]
 * - after calling it, when converting the return value
 *   and out arguments [out]
 * - at the end of the invocation, to release any
 *   allocated memory [release]
 *
 * The convention on the names is thus
 * gjs_marshal_[argument type]_[direction]_[phase].
 * Some types don't have direction (for example, caller_allocates
 * is only out, and callback is only in), in which case it is
 * implied.
 */

static JSBool
gjs_marshal_skipped_in (JSContext            *cx,
                        GjsArgumentCache     *self,
                        GjsFunctionCallState *state,
                        GArgument            *arg,
                        jsval                 value)
{
    return JS_TRUE;
}

static JSBool
gjs_marshal_generic_in_in (JSContext            *cx,
                           GjsArgumentCache     *self,
                           GjsFunctionCallState *state,
                           GArgument            *arg,
                           jsval                 value)
{
    return gjs_value_to_g_argument(cx, value,
                                   &self->type_info,
                                   self->arg_name,
                                   self->is_return ?
                                   GJS_ARGUMENT_RETURN_VALUE :
                                   GJS_ARGUMENT_ARGUMENT,
                                   self->transfer,
                                   self->nullable,
                                   arg);
}

static JSBool
gjs_marshal_generic_inout_in (JSContext           *cx,
                              GjsArgumentCache     *self,
                              GjsFunctionCallState *state,
                              GArgument            *arg,
                              jsval                 value)
{
    if (!gjs_marshal_generic_in_in (cx, self, state, arg, value))
        return JS_FALSE;

    state->out_arg_cvalues[self->arg_index] = state->inout_original_arg_cvalues[self->arg_index] = *arg;
    arg->v_pointer = &(state->out_arg_cvalues[self->arg_index]);
    return JS_TRUE;
}

static JSBool
gjs_marshal_explicit_array_in_in (JSContext            *cx,
                                  GjsArgumentCache     *self,
                                  GjsFunctionCallState *state,
                                  GArgument            *arg,
                                  jsval                 value)
{
    gpointer data;
    gsize length;

    if (!gjs_array_to_explicit_array(cx, value,
                                     &self->type_info,
                                     self->arg_name,
                                     GJS_ARGUMENT_ARGUMENT,
                                     self->transfer,
                                     self->nullable,
                                     &data, &length))
        return JS_FALSE;

    gjs_g_argument_set_ulong(self->contents.array.length_tag,
                             &state->in_arg_cvalues[self->contents.array.length_arg],
                             length);
    arg->v_pointer = data;
    return JS_TRUE;
}

static JSBool
gjs_marshal_explicit_array_inout_in (JSContext            *cx,
                                     GjsArgumentCache     *self,
                                     GjsFunctionCallState *state,
                                     GArgument            *arg,
                                     jsval                 value)
{
    int array_length_pos;

    if (!gjs_marshal_explicit_array_in_in(cx, self, state, arg, value))
        return JS_FALSE;

    array_length_pos = self->contents.array.length_arg;

    if (arg->v_pointer == NULL) {
        /* Special case where we were given JS null to
         * also pass null for length, and not a
         * pointer to an integer that derefs to 0.
         */
        state->in_arg_cvalues[array_length_pos].v_pointer = NULL;
        state->out_arg_cvalues[array_length_pos].v_int = 0;
        state->inout_original_arg_cvalues[array_length_pos].v_int = 0;

        state->out_arg_cvalues[self->arg_index].v_pointer = state->inout_original_arg_cvalues[self->arg_index].v_pointer = NULL;
    } else {
        state->out_arg_cvalues[array_length_pos] = state->inout_original_arg_cvalues[array_length_pos] = state->in_arg_cvalues[array_length_pos];
        state->in_arg_cvalues[array_length_pos].v_pointer = &state->out_arg_cvalues[array_length_pos];

        state->out_arg_cvalues[self->arg_index] = state->inout_original_arg_cvalues[self->arg_index] = *arg;
        arg->v_pointer = &(state->out_arg_cvalues[self->arg_index]);
    }

    return JS_TRUE;
}

static JSBool
gjs_marshal_callback_in (JSContext            *cx,
                         GjsArgumentCache     *self,
                         GjsFunctionCallState *state,
                         GArgument            *arg,
                         jsval                 value)
{
    GICallableInfo *callable_info;
    GjsCallbackTrampoline *trampoline;
    ffi_closure *closure;
    int destroy_arg, closure_arg;

    if (JSVAL_IS_NULL(value) && self->nullable) {
        closure = NULL;
        trampoline = NULL;
    } else {
        if (!(JS_TypeOfValue(cx, value) == JSTYPE_FUNCTION)) {
            gjs_throw(cx, "Expected function for callback argument %s, got %s",
                      self->arg_name,
                      JS_GetTypeName(cx, JS_TypeOfValue(cx, value)));
            return JS_FALSE;
        }

        callable_info = (GICallableInfo*) g_type_info_get_interface(&self->type_info);
        trampoline = gjs_callback_trampoline_new(cx,
                                                 value,
                                                 callable_info,
                                                 self->contents.callback.scope,
                                                 /* FIXME: is_object_method ? obj : NULL */
                                                 NULL,
                                                 FALSE);
        closure = trampoline->closure;
        g_base_info_unref(callable_info);
    }

    destroy_arg = self->contents.callback.destroy;
    if (destroy_arg >= 0) {
        state->in_arg_cvalues[destroy_arg].v_pointer = trampoline ? gjs_destroy_notify_callback : NULL;
    }
    closure_arg = self->contents.callback.closure;
    if (closure_arg >= 0) {
        state->in_arg_cvalues[closure_arg].v_pointer = trampoline;
    }

    if (trampoline && self->contents.callback.scope != GI_SCOPE_TYPE_CALL) {
        /* Add an extra reference that will be cleared when collecting
           async calls, or when GDestroyNotify is called */
        gjs_callback_trampoline_ref(trampoline);
    }
    arg->v_pointer = closure;

    return JS_TRUE;
}

static JSBool
gjs_marshal_generic_out_in (JSContext            *cx,
                            GjsArgumentCache     *self,
                            GjsFunctionCallState *state,
                            GArgument            *arg,
                            jsval                 value)
{
    arg->v_pointer = &state->out_arg_cvalues[self->arg_index];
    return JS_TRUE;
}

static JSBool
gjs_marshal_caller_allocates_in (JSContext            *cx,
                                 GjsArgumentCache     *self,
                                 GjsFunctionCallState *state,
                                 GArgument            *arg,
                                 jsval                 value)
{
    gpointer blob;

    blob = g_slice_alloc0(self->contents.caller_allocates_size);
    arg->v_pointer = blob;
    state->out_arg_cvalues[self->arg_index].v_pointer = blob;

    return JS_TRUE;
}

static JSBool
gjs_marshal_skipped_out (JSContext            *cx,
                         GjsArgumentCache     *self,
                         GjsFunctionCallState *state,
                         GArgument            *arg,
                         jsval                *value)
{
    return JS_TRUE;
}

static JSBool
gjs_marshal_generic_out_out (JSContext            *cx,
                             GjsArgumentCache     *self,
                             GjsFunctionCallState *state,
                             GArgument            *arg,
                             jsval                *value)
{
    return gjs_value_from_g_argument(cx, value,
                                     &self->type_info,
                                     arg, TRUE);
}

static JSBool
gjs_marshal_explicit_array_out_out (JSContext            *cx,
                                    GjsArgumentCache     *self,
                                    GjsFunctionCallState *state,
                                    GArgument            *arg,
                                    jsval                *value)
{
    GArgument *length_arg;
    GITypeTag length_tag;
    gsize length;

    length_arg = &(state->out_arg_cvalues[self->contents.array.length_arg]);
    length_tag = self->contents.array.length_tag;
    length = gjs_g_argument_get_ulong(length_tag, length_arg);

    return gjs_value_from_explicit_array(cx, value,
                                         &self->type_info,
                                         arg, length);
}

static JSBool
gjs_marshal_skipped_release (JSContext            *cx,
                             GjsArgumentCache     *self,
                             GjsFunctionCallState *state,
                             GArgument            *in_arg,
                             GArgument            *out_arg)
{
    return JS_TRUE;
}

static JSBool
gjs_marshal_generic_in_release (JSContext            *cx,
                                GjsArgumentCache     *self,
                                GjsFunctionCallState *state,
                                GArgument            *in_arg,
                                GArgument            *out_arg)
{
    return gjs_g_argument_release_in_arg(cx, self->transfer,
                                         &self->type_info, in_arg);
}

static JSBool
gjs_marshal_generic_out_release (JSContext            *cx,
                                 GjsArgumentCache     *self,
                                 GjsFunctionCallState *state,
                                 GArgument            *in_arg,
                                 GArgument            *out_arg)
{
    return gjs_g_argument_release(cx, self->transfer,
                                  &self->type_info, out_arg);
}

static JSBool
gjs_marshal_generic_inout_release (JSContext            *cx,
                                   GjsArgumentCache     *self,
                                   GjsFunctionCallState *state,
                                   GArgument            *in_arg,
                                   GArgument            *out_arg)
{
    GArgument *original_out_arg;
    /* For inout, transfer refers to what we get back from the function; for
     * the temporary C value we allocated, clearly we're responsible for
     * freeing it.
     */

    original_out_arg = &(state->inout_original_arg_cvalues[self->arg_index]);
    if (!gjs_g_argument_release_in_arg(cx, GI_TRANSFER_NOTHING,
                                       &self->type_info, original_out_arg))
        return JS_FALSE;

    return gjs_marshal_generic_out_release(cx, self, state, in_arg, out_arg);
}

static JSBool
gjs_marshal_explicit_array_out_release (JSContext            *cx,
                                        GjsArgumentCache     *self,
                                        GjsFunctionCallState *state,
                                        GArgument            *in_arg,
                                        GArgument            *out_arg)
{
    GArgument *length_arg;
    GITypeTag length_tag;
    gsize length;

    length_arg = &(state->out_arg_cvalues[self->contents.array.length_arg]);
    length_tag = self->contents.array.length_tag;
    length = gjs_g_argument_get_ulong(length_tag, length_arg);

    return gjs_g_argument_release_out_array(cx, self->transfer,
                                            &self->type_info,
                                            length, out_arg);
}

static JSBool
gjs_marshal_explicit_array_in_release (JSContext            *cx,
                                       GjsArgumentCache     *self,
                                       GjsFunctionCallState *state,
                                       GArgument            *in_arg,
                                       GArgument            *out_arg)
{
    GArgument *length_arg;
    GITypeTag length_tag;
    gsize length;

    length_arg = &(state->in_arg_cvalues[self->contents.array.length_arg]);
    length_tag = self->contents.array.length_tag;
    length = gjs_g_argument_get_ulong(length_tag, length_arg);

    return gjs_g_argument_release_in_array(cx, self->transfer,
                                           &self->type_info,
                                           length, in_arg);
}

static JSBool
gjs_marshal_explicit_array_inout_release (JSContext            *cx,
                                          GjsArgumentCache     *self,
                                          GjsFunctionCallState *state,
                                          GArgument            *in_arg,
                                          GArgument            *out_arg)
{
    GArgument *original_out_arg;
    GArgument *length_arg;
    GITypeTag length_tag;
    gsize length;

    length_arg = &(state->in_arg_cvalues[self->contents.array.length_arg]);
    length_tag = self->contents.array.length_tag;
    length = gjs_g_argument_get_ulong(length_tag, length_arg);

    /* For inout, transfer refers to what we get back from the function; for
     * the temporary C value we allocated, clearly we're responsible for
     * freeing it.
     */

    original_out_arg = &(state->inout_original_arg_cvalues[self->arg_index]);
    if (original_out_arg->v_pointer != out_arg->v_pointer) {
        if (!gjs_g_argument_release_in_array(cx, GI_TRANSFER_NOTHING,
                                             &self->type_info,
                                             length, original_out_arg))
            return JS_FALSE;
    }

    return gjs_g_argument_release_out_array(cx, self->transfer,
                                            &self->type_info,
                                            length, out_arg);
}

static JSBool
gjs_marshal_caller_allocates_release (JSContext            *cx,
                                      GjsArgumentCache     *self,
                                      GjsFunctionCallState *state,
                                      GArgument            *in_arg,
                                      GArgument            *out_arg)
{
    g_slice_free1(self->contents.caller_allocates_size, in_arg->v_pointer);
    return JS_TRUE;
}

static JSBool
gjs_marshal_callback_release (JSContext            *cx,
                              GjsArgumentCache     *self,
                              GjsFunctionCallState *state,
                              GArgument            *in_arg,
                              GArgument            *out_arg)
{
    ffi_closure *closure = in_arg->v_pointer;

    if (closure) {
        GjsCallbackTrampoline *trampoline = closure->user_data;
        /* CallbackTrampolines are refcounted because for notified/async closures
           it is possible to destroy it while in call, and therefore we cannot check
           its scope at this point */
        gjs_callback_trampoline_unref(trampoline);
        in_arg->v_pointer = NULL;
    }

    return JS_TRUE;
}

static inline void
gjs_arg_cache_set_skip_all (GjsArgumentCache *self)
{
    self->marshal_in = gjs_marshal_skipped_in;
    self->marshal_out = gjs_marshal_skipped_out;
    self->release = gjs_marshal_skipped_release;
    self->skip_in = self->skip_out = TRUE;
}

JSBool
gjs_arg_cache_build_return (GjsArgumentCache *self,
                            GjsArgumentCache *arguments,
                            GICallableInfo   *info,
                            JSBool           *inc_counter)
{
    GITypeInfo return_type;
    int array_length_pos;

    g_callable_info_load_return_type(info, &return_type);

    if (g_type_info_get_tag(&return_type) == GI_TYPE_TAG_VOID) {
        *inc_counter = FALSE;
        gjs_arg_cache_set_skip_all(self);
        return JS_TRUE;
    }

    *inc_counter = TRUE;
    self->arg_index = -1;
    self->arg_name = "return value";
    g_callable_info_load_return_type(info, &self->type_info);
    self->transfer = g_callable_info_get_caller_owns(info);
    self->nullable = FALSE; /* We don't really care for return values */
    self->is_return = TRUE;

    if (g_type_info_get_tag(&self->type_info) == GI_TYPE_TAG_ARRAY) {
        array_length_pos = g_type_info_get_array_length(&return_type);
        if (array_length_pos >= 0) {
            GIArgInfo array_length_arg;
            GITypeInfo array_length_type;

            gjs_arg_cache_set_skip_all(&arguments[array_length_pos]);

            /* Even if we skip the length argument most of time,
               we need to do some basic initialization here.
            */
            arguments[array_length_pos].arg_index = array_length_pos;
            arguments[array_length_pos].marshal_in = gjs_marshal_generic_out_in;

            self->marshal_in = gjs_marshal_generic_out_in;
            self->marshal_out = gjs_marshal_explicit_array_out_out;
            self->release = gjs_marshal_explicit_array_out_release;

            self->contents.array.length_arg = array_length_pos;

            g_callable_info_load_arg(info, array_length_pos, &array_length_arg);
            g_arg_info_load_type(&array_length_arg, &array_length_type);
            self->contents.array.length_tag = g_type_info_get_tag(&array_length_type);

            return JS_TRUE;
        }
    }

    /* marshal_in is ignored for the return value, but skip_in is not
       (it is used in the failure release path) */
    self->skip_in = TRUE;
    self->marshal_out = gjs_marshal_generic_out_out;
    self->release = gjs_marshal_generic_out_release;

    return JS_TRUE;
}

JSBool
gjs_arg_cache_build_arg (GjsArgumentCache *self,
                         GjsArgumentCache *arguments,
                         int               gi_index,
                         GIDirection       direction,
                         GIArgInfo        *arg_info,
                         GICallableInfo   *callable,
                         JSBool           *inc_counter)
{
    GITypeInfo type_info;
    int destroy = -1;
    int closure = -1;
    int array_length_pos;
    GITypeTag type_tag;

    g_arg_info_load_type(arg_info, &type_info);

    self->arg_index = gi_index;
    self->arg_name = g_base_info_get_name((GIBaseInfo*) arg_info);
    g_arg_info_load_type(arg_info, &self->type_info);
    self->transfer = g_arg_info_get_ownership_transfer(arg_info);
    self->nullable = g_arg_info_may_be_null(arg_info);
    self->is_return = FALSE;

    if (direction == GI_DIRECTION_IN)
        self->skip_out = TRUE;
    else if (direction == GI_DIRECTION_OUT)
        self->skip_in = TRUE;
    *inc_counter = TRUE;

    if (direction == GI_DIRECTION_OUT &&
        g_arg_info_is_caller_allocates(arg_info)) {
        GIInterfaceInfo *interface_info;
        GIInfoType interface_type;
        gsize size;

        interface_info = g_type_info_get_interface(&type_info);
        g_assert(interface_info != NULL);

        interface_type = g_base_info_get_type(interface_info);

        if (interface_type == GI_INFO_TYPE_STRUCT) {
            size = g_struct_info_get_size((GIStructInfo*)interface_info);
        } else if (interface_type == GI_INFO_TYPE_UNION) {
            size = g_union_info_get_size((GIUnionInfo*)interface_info);
        } else {
            /* Can't do caller allocates on anything else */

            g_base_info_unref((GIBaseInfo*)interface_info);
            return JS_FALSE;
        }

        g_base_info_unref((GIBaseInfo*)interface_info);

        self->marshal_in = gjs_marshal_caller_allocates_in;
        self->marshal_out = gjs_marshal_generic_out_out;
        self->release = gjs_marshal_caller_allocates_release;
        self->contents.caller_allocates_size = size;

        return JS_TRUE;
    }

    type_tag = g_type_info_get_tag(&type_info);

    if (type_tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo* interface_info;
        GIInfoType interface_type;

        interface_info = g_type_info_get_interface(&type_info);
        interface_type = g_base_info_get_type(interface_info);
        if (interface_type == GI_INFO_TYPE_CALLBACK) {
            if (direction != GI_DIRECTION_IN) {
                /* Can't do callbacks for out or inout */
                g_base_info_unref(interface_info);
                return JS_FALSE;
            }

            if (strcmp(g_base_info_get_name(interface_info), "DestroyNotify") == 0 &&
                strcmp(g_base_info_get_namespace(interface_info), "GLib") == 0) {
                /* Skip GDestroyNotify if they appear before the respective callback */
                gjs_arg_cache_set_skip_all(self);
                *inc_counter = FALSE;
            } else {
                self->marshal_in = gjs_marshal_callback_in;
                self->marshal_out = gjs_marshal_skipped_out;
                self->release = gjs_marshal_callback_release;

                destroy = g_arg_info_get_destroy(arg_info);
                closure = g_arg_info_get_closure(arg_info);

                if (destroy >= 0)
                    gjs_arg_cache_set_skip_all(&arguments[destroy]);

                if (closure >= 0)
                    gjs_arg_cache_set_skip_all(&arguments[closure]);

                if (destroy >= 0 && closure < 0) {
                    /* Function has a GDestroyNotify but no user_data, not supported */
                    g_base_info_unref(interface_info);
                    return JS_FALSE;
                }

                self->contents.callback.scope = g_arg_info_get_scope(arg_info);
                self->contents.callback.destroy = destroy;
                self->contents.callback.closure = closure;
            }

            g_base_info_unref(interface_info);
            return JS_TRUE;
        }

        g_base_info_unref(interface_info);
    }

    if (type_tag == GI_TYPE_TAG_ARRAY &&
        g_type_info_get_array_type(&type_info) == GI_ARRAY_TYPE_C) {
        array_length_pos = g_type_info_get_array_length(&type_info);

        if (array_length_pos >= 0) {
            GIArgInfo array_length_arg;
            GITypeInfo array_length_type;

            gjs_arg_cache_set_skip_all(&arguments[array_length_pos]);

            if (direction == GI_DIRECTION_IN) {
                self->marshal_in = gjs_marshal_explicit_array_in_in;
                self->marshal_out = gjs_marshal_skipped_out;
                self->release = gjs_marshal_explicit_array_in_release;
            } else if (direction == GI_DIRECTION_INOUT) {
                self->marshal_in = gjs_marshal_explicit_array_inout_in;
                self->marshal_out = gjs_marshal_explicit_array_out_out;
                self->release = gjs_marshal_explicit_array_inout_release;
            } else {
                /* Even if we skip the length argument most of time,
                   we need to do some basic initialization here.
                */
                arguments[array_length_pos].arg_index = array_length_pos;
                arguments[array_length_pos].marshal_in = gjs_marshal_generic_out_in;

                self->marshal_in = gjs_marshal_generic_out_in;
                self->marshal_out = gjs_marshal_explicit_array_out_out;
                self->release = gjs_marshal_explicit_array_out_release;
            }

            self->contents.array.length_arg = array_length_pos;

            g_callable_info_load_arg(callable, array_length_pos, &array_length_arg);
            g_arg_info_load_type(&array_length_arg, &array_length_type);
            self->contents.array.length_tag = g_type_info_get_tag(&array_length_type);

            if (array_length_pos < gi_index) {
                /* we already collected array_length_pos, remove it */
                *inc_counter = FALSE;
            }

            return JS_TRUE;
        }
    }

    if (direction == GI_DIRECTION_IN) {
        gjs_arg_cache_build_normal_in_arg(self, type_tag);
        self->marshal_out = gjs_marshal_skipped_out;
    } else if (direction == GI_DIRECTION_INOUT) {
        self->marshal_in = gjs_marshal_generic_inout_in;
        self->marshal_out = gjs_marshal_generic_out_out;
        self->release = gjs_marshal_generic_inout_release;
    } else {
        self->marshal_in = gjs_marshal_generic_out_in;
        self->marshal_out = gjs_marshal_generic_out_out;
        self->release = gjs_marshal_generic_out_release;
    }

    return JS_TRUE;
}

static JSBool
report_primitive_type_mismatch(JSContext        *cx,
                               GjsArgumentCache *self,
                               jsval             value,
                               JSType            expected)
{
    gjs_throw(cx, "Expected type %s for argument '%s' but got type %s",
              JS_GetTypeName(cx, expected), self->arg_name,
              JS_GetTypeName(cx, JS_TypeOfValue(cx, value)));
    return JS_FALSE;
}

static JSBool
report_out_of_range(JSContext        *cx,
                    GjsArgumentCache *self,
                    GITypeTag         tag)
{
    gjs_throw(cx, "Argument %s: value is out of range for %s",
              self->arg_name, g_type_tag_to_string(tag));
    return JS_FALSE;
}

static JSBool
report_invalid_null(JSContext        *cx,
                    GjsArgumentCache *self)
{
    gjs_throw(cx, "Argument %s may not be null", self->arg_name);
    return JS_FALSE;
}

static JSBool
gjs_marshal_null_in_in (JSContext            *cx,
                        GjsArgumentCache     *self,
                        GjsFunctionCallState *state,
                        GArgument            *arg,
                        jsval                 value)
{
    arg->v_pointer = NULL;
    return JS_TRUE;
}

static JSBool
gjs_marshal_boolean_in_in (JSContext            *cx,
                           GjsArgumentCache     *self,
                           GjsFunctionCallState *state,
                           GArgument            *arg,
                           jsval                 value)
{
    JSBool b;

    if (!JS_ValueToBoolean(cx, value, &b))
        return JS_FALSE;

    arg->v_boolean = b;
    return JS_TRUE;
}

/* Type tags are alternated, signed / unsigned */
static gint32 min_max_ints[5][2] = {
    { G_MININT8,  G_MAXINT8 },
    { 0,          G_MAXUINT8 },
    { G_MININT16, G_MAXINT16 },
    { 0,          G_MAXUINT16 },
    { G_MININT32, G_MAXINT32 }
};

static inline JSBool
value_in_range(gint32    number,
               GITypeTag tag)
{
    return (number >= min_max_ints[tag - GI_TYPE_TAG_INT8][0] &&
            number <= min_max_ints[tag - GI_TYPE_TAG_INT8][1]);
}

static JSBool
gjs_marshal_integer_in_in (JSContext            *cx,
                           GjsArgumentCache     *self,
                           GjsFunctionCallState *state,
                           GArgument            *arg,
                           jsval                 value)
{
    GITypeTag tag;

    tag = self->contents.number.number_tag;

    if (self->contents.number.is_unsigned) {
        guint32 number;

        if (!JS_ValueToECMAUint32(cx, value, &number))
            return JS_FALSE;

        if (!value_in_range(number, tag))
            return report_out_of_range(cx, self, tag);

        gjs_g_argument_set_ulong(tag, arg, number);
    } else {
        gint32 number;

        /* XXX:
           The ToInt32() algorithm of ES5 says that conversion
           of NaN (which include almost everything) produces 0,
           but our test suite assumes that passing objects as
           number fails, so we use the legacy conversion here.
        */
        if (!JS_ValueToInt32(cx, value, &number))
            return JS_FALSE;

        if (!value_in_range(number, tag))
            return report_out_of_range(cx, self, tag);

        gjs_g_argument_set_ulong(tag, arg, number);
    }

    return JS_TRUE;
}

static JSBool
gjs_marshal_number_in_in (JSContext            *cx,
                          GjsArgumentCache     *self,
                          GjsFunctionCallState *state,
                          GArgument            *arg,
                          jsval                 value)
{
    double v;

    if (!JS_ValueToNumber(cx, value, &v))
        return JS_FALSE;

    switch (self->contents.number.number_tag) {
    case GI_TYPE_TAG_DOUBLE:
        arg->v_double = v;
        break;
    case GI_TYPE_TAG_FLOAT:
        if (v < - G_MAXFLOAT || v > G_MAXFLOAT)
            return report_out_of_range(cx, self, GI_TYPE_TAG_FLOAT);

        arg->v_float = v;
        break;
    case GI_TYPE_TAG_INT64:
        if (v < G_MININT64 || v > G_MAXINT64)
            return report_out_of_range(cx, self, GI_TYPE_TAG_INT64);

        arg->v_int64 = v;
        break;
    case GI_TYPE_TAG_UINT64:
        if (v < 0 || v > G_MAXUINT64)
            return report_out_of_range(cx, self, GI_TYPE_TAG_UINT64);

        arg->v_uint64 = v;
        break;
    case GI_TYPE_TAG_UINT32:
        if (v < 0 || v > G_MAXUINT32)
            return report_out_of_range(cx, self, GI_TYPE_TAG_UINT32);

        arg->v_uint32 = v;
        break;

    default:
        g_assert_not_reached();
    }

    return JS_TRUE;
}

static JSBool
gjs_marshal_unichar_in_in (JSContext            *cx,
                           GjsArgumentCache     *self,
                           GjsFunctionCallState *state,
                           GArgument            *arg,
                           jsval                 value)
{
    if (!JSVAL_IS_STRING(value))
        return report_primitive_type_mismatch(cx, self, value, JSTYPE_STRING);

    return gjs_unichar_from_string(cx, value, &arg->v_uint32);
}

static JSBool
gjs_marshal_gtype_in_in (JSContext            *cx,
                         GjsArgumentCache     *self,
                         GjsFunctionCallState *state,
                         GArgument            *arg,
                         jsval                 value)
{
    if (!JSVAL_IS_OBJECT(value))
        return report_primitive_type_mismatch(cx, self, value, JSTYPE_OBJECT);
    if (JSVAL_IS_NULL(value))
        return report_invalid_null(cx, self);

    arg->v_ssize = gjs_gtype_get_actual_gtype(cx, JSVAL_TO_OBJECT(value));
    return arg->v_ssize != G_TYPE_INVALID;
}

static JSBool
gjs_marshal_string_in_in (JSContext            *cx,
                          GjsArgumentCache     *self,
                          GjsFunctionCallState *state,
                          GArgument            *arg,
                          jsval                 value)
{
    char *str;
    JSBool ok;

    if (JSVAL_IS_NULL(value)) {
        if (!self->nullable)
            return report_invalid_null(cx, self);

        arg->v_pointer = NULL;
        return JS_TRUE;
    }

    if (!JSVAL_IS_STRING(value))
        return report_primitive_type_mismatch(cx, self, value, JSTYPE_STRING);

    if (self->contents.string_is_filename)
        ok = gjs_string_to_filename(cx, value, &str);
    else
        ok = gjs_string_to_utf8(cx, value, &str);

    arg->v_pointer = str;
    return ok;
}

static JSBool
gjs_marshal_string_in_release (JSContext            *cx,
                               GjsArgumentCache     *self,
                               GjsFunctionCallState *state,
                               GArgument            *in_arg,
                               GArgument            *out_arg)
{
    g_free(in_arg->v_pointer);
    return JS_TRUE;
}

static JSBool
gjs_arg_cache_build_normal_in_arg (GjsArgumentCache *self,
                                   GITypeTag         tag)
{
    /* "Normal" in arguments are those arguments that don't
       require special processing, and don't touch other
       arguments.
       Main categories are:
       - void*
       - small numbers (fit in 32bit)
       - big numbers (need a double)
       - strings
       - enums/flags (different from numbers in the way
         they're exposed in GI)
       - objects (GObjects, boxed, unions, etc.)
       - hashes
       - sequences (null-terminated arrays, lists, etc.)
    */

    self->release = gjs_marshal_skipped_release;

    switch (tag) {
    case GI_TYPE_TAG_VOID:
        self->marshal_in = gjs_marshal_null_in_in;
        break;

    case GI_TYPE_TAG_BOOLEAN:
        self->marshal_in = gjs_marshal_boolean_in_in;
        break;

    case GI_TYPE_TAG_INT8:
    case GI_TYPE_TAG_INT16:
    case GI_TYPE_TAG_INT32:
        self->marshal_in = gjs_marshal_integer_in_in;
        self->contents.number.number_tag = tag;
        self->contents.number.is_unsigned = FALSE;
        break;

    case GI_TYPE_TAG_UINT8:
    case GI_TYPE_TAG_UINT16:
        self->marshal_in = gjs_marshal_integer_in_in;
        self->contents.number.number_tag = tag;
        self->contents.number.is_unsigned = TRUE;
        break;

    case GI_TYPE_TAG_UINT32:
    case GI_TYPE_TAG_INT64:
    case GI_TYPE_TAG_UINT64:
    case GI_TYPE_TAG_FLOAT:
    case GI_TYPE_TAG_DOUBLE:
        self->marshal_in = gjs_marshal_number_in_in;
        self->contents.number.number_tag = tag;
        break;

    case GI_TYPE_TAG_UNICHAR:
        self->marshal_in = gjs_marshal_unichar_in_in;
        break;

    case GI_TYPE_TAG_GTYPE:
        self->marshal_in = gjs_marshal_gtype_in_in;
        break;

    case GI_TYPE_TAG_FILENAME:
        self->marshal_in = gjs_marshal_string_in_in;
        if (self->transfer != GI_TRANSFER_NOTHING)
            self->release = gjs_marshal_string_in_release;
        self->contents.string_is_filename = TRUE;
        break;

    case GI_TYPE_TAG_UTF8:
        self->marshal_in = gjs_marshal_string_in_in;
        if (self->transfer != GI_TRANSFER_NOTHING)
            self->release = gjs_marshal_string_in_release;
        self->contents.string_is_filename = FALSE;
        break;

    default:
        /* FIXME */
        /* Falling back to the generic marshaller */
        self->marshal_in = gjs_marshal_generic_in_in;
        self->release = gjs_marshal_generic_in_release;
    }

    return JS_TRUE;
}
