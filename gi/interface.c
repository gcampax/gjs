/* -*- mode: C; c-basic-offset: 4; indent-tabs-mode: nil; -*- */
/*
 * Copyright (c) 2008  litl, LLC
 * Copyright (c) 2012  Red Hat, Inc.
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
#include "gtype.h"
#include "interface.h"

#include <gjs/gjs-module.h>
#include <gjs/compat.h>
#include <util/log.h>

#include <jsapi.h>
#include <girepository.h>

typedef struct {
    GIInterfaceInfo *info;
    GType gtype;
} Interface;

static struct JSClass gjs_interface_class;

GJS_DEFINE_DYNAMIC_PRIV_FROM_JS(Interface, gjs_interface_class)

GJS_NATIVE_CONSTRUCTOR_DECLARE(interface)
{
    jsval obj;
    JSObject *proto;
    JSObject *constructor;
    Interface *priv;

    constructor = JSVAL_TO_OBJECT(JS_CALLEE(context, vp));
    gjs_object_get_property(context, constructor, "prototype", &obj);
    proto = JSVAL_TO_OBJECT(obj);
    priv = priv_from_js(context, proto);

    gjs_throw(context, "You cannot construct new instances of '%s.%s'",
              g_base_info_get_namespace(priv->info),
              g_base_info_get_name(priv->info));

    return JS_TRUE;
}

static void
interface_finalize(JSContext *context,
                   JSObject  *obj)
{
    Interface *priv;

    priv = priv_from_js(context, obj);

    if (priv == NULL)
        return;

    if (priv->info != NULL)
        g_base_info_unref((GIBaseInfo*)priv->info);

    GJS_DEC_COUNTER(interface);
    g_slice_free(Interface, priv);
}

static JSBool
gjs_define_static_methods(JSContext       *context,
                          JSObject        *constructor,
                          GType            gtype,
                          GIInterfaceInfo *info)
{
    int i;
    int n_methods;

    n_methods = g_interface_info_get_n_methods(info);

    for (i = 0; i < n_methods; i++) {
        GIFunctionInfo *meth_info;
        GIFunctionInfoFlags flags;

        meth_info = g_interface_info_get_method (info, i);
        flags = g_function_info_get_flags (meth_info);

        /* Anything that isn't a method we put on the prototype of the
         * constructor.  This includes <constructor> introspection
         * methods, as well as the forthcoming "static methods"
         * support.  We may want to change this to use
         * GI_FUNCTION_IS_CONSTRUCTOR and GI_FUNCTION_IS_STATIC or the
         * like in the near future.
         */
        if (!(flags & GI_FUNCTION_IS_METHOD)) {
            gjs_define_function(context, constructor, gtype,
                                (GICallableInfo *)meth_info);
        }

        g_base_info_unref((GIBaseInfo*) meth_info);
    }
    return JS_TRUE;
}

static JSBool
interface_new_resolve(JSContext *context,
                      JSObject  *obj,
                      jsid       id,
                      uintN      flags,
                      JSObject **objp)
{
    Interface *priv;
    char *name;
    JSBool ret = JS_FALSE;
    GIFunctionInfo *method_info;

    *objp = NULL;

    if (!gjs_get_string_id(context, id, &name))
        return JS_TRUE;

    priv = priv_from_js(context, obj);

    if (priv == NULL)
        goto out;

    method_info = g_interface_info_find_method((GIInterfaceInfo*) priv->info, name);

    if (method_info != NULL) {
        if (gjs_define_function(context, obj,
                                priv->gtype,
                                (GICallableInfo*)method_info) == NULL) {
            g_base_info_unref((GIBaseInfo*)method_info);
            goto out;
        }

        *objp = obj;
        g_base_info_unref((GIBaseInfo*)method_info);
    }

    ret = JS_TRUE;

 out:
    g_free (name);
    return ret;
}

static struct JSClass gjs_interface_class = {
    NULL, /* dynamic */
    JSCLASS_HAS_PRIVATE |
    JSCLASS_NEW_RESOLVE |
    JSCLASS_NEW_RESOLVE_GETS_START,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_StrictPropertyStub,
    JS_EnumerateStub,
    (JSResolveOp) interface_new_resolve,
    JS_ConvertStub,
    interface_finalize,
    NULL,
    NULL,
    NULL,
    NULL, NULL, NULL, NULL, NULL
};

static JSPropertySpec gjs_interface_proto_props[] = {
    { NULL }
};

static JSFunctionSpec gjs_interface_proto_funcs[] = {
    { NULL }
};

static JSBool
interface_constructor_has_instance(JSContext   *context,
                                   JSObject    *self,
                                   const jsval *v,
                                   JSBool      *p_bool)
{
    JSObject *proto;
    jsval v_proto;
    Interface *priv;
    GType obj_gtype;

    /* Looking at code, doesn't seem to be possible, but
       better safe than sorry */
    if (!JSVAL_IS_OBJECT(*v))
        return JS_FALSE;

    if (!gjs_object_get_property(context, self,
                                 "prototype", &v_proto))
        return JS_FALSE;

    proto = JSVAL_TO_OBJECT(v_proto);
    priv = priv_from_js(context, proto);

    /* This will go through .constructor as necessary to
       retrieve the GType for an object */
    obj_gtype = gjs_gtype_get_actual_gtype(context, JSVAL_TO_OBJECT(*v));

    *p_bool = g_type_is_a(obj_gtype, priv->gtype);

    return JS_TRUE;
}

static struct JSClass gjs_interface_constructor_class = {
    "GjsInterfaceConstructor",
    0, /* flags */
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_StrictPropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    NULL, /* finalize */
    NULL, /* reserved */
    NULL, /* checkAccess */
    NULL, /* call */
    NULL, /* construct */
    NULL, /* xdrInstance */
    interface_constructor_has_instance,
};

static JSObject *
make_custom_constructor(JSContext *context,
                        JSObject  *prototype)
{
    JSObject *retval;
    jsval v;
    static gboolean has_class = FALSE;

    if (G_UNLIKELY(!has_class)) {
        JSObject *global;

        global = gjs_get_import_global(context);

        JS_InitClass(context, global,
                     NULL, /* parent_proto */
                     &gjs_interface_constructor_class,
                     NULL, /* constructor (irrelevant because the class invisible to code) */
                     0, /* nargs */
                     NULL, NULL, NULL, NULL);

        has_class = TRUE;
    }

    retval = JS_NewObject(context, &gjs_interface_constructor_class,
                          NULL /* proto */, NULL /* parent */);

    /* The first operation is equivalent to rooting retval, as prototype
       is kept alive by the real constructor */
    v = OBJECT_TO_JSVAL(retval);
    JS_SetProperty(context, prototype, "constructor", &v);

    v = OBJECT_TO_JSVAL(prototype);
    JS_SetProperty(context, retval, "prototype", &v);

    return retval;
 }

JSBool
gjs_define_interface_class(JSContext       *context,
                           JSObject        *in_object,
                           GIInterfaceInfo *info,
                           JSObject       **prototype_p)
{
    Interface *priv;
    const char *constructor_name;
    JSObject *constructor;
    JSObject *prototype;
    jsval value;

    constructor_name = g_base_info_get_name((GIBaseInfo*)info);

    gjs_object_get_property(context, in_object, constructor_name, &value);
    if (value != JSVAL_VOID) {
        JSObject *constructor;

        if (!JSVAL_IS_OBJECT(value)) {
            gjs_throw(context, "Existing property '%s' does not look like a constructor",
                      constructor_name);
            return JS_FALSE;
        }

        constructor = JSVAL_TO_OBJECT(value);

        gjs_object_get_property(context, constructor, "prototype", &value);
        if (!JSVAL_IS_OBJECT(value)) {
            gjs_throw(context, "prototype property does not appear to exist or has wrong type");
            return JS_FALSE;
        } else {
            if (prototype_p)
                *prototype_p = JSVAL_TO_OBJECT(value);

            return JS_TRUE;
        }

        return JS_TRUE;
    }

    prototype = gjs_init_class_dynamic(context, in_object,
                                       /* parent prototype JSObject* for
                                        * prototype; NULL for
                                        * Object.prototype
                                        */
                                       NULL,
                                       g_base_info_get_namespace((GIBaseInfo*)info),
                                       constructor_name,
                                       &gjs_interface_class,
                                       /* constructor for instances (NULL for
                                        * none - just name the prototype like
                                        * Math - rarely correct)
                                        */
                                       gjs_interface_constructor,
                                       /* number of constructor args */
                                       0,
                                       /* props of prototype */
                                       &gjs_interface_proto_props[0],
                                       /* funcs of prototype */
                                       &gjs_interface_proto_funcs[0],
                                       /* props of constructor, MyConstructor.myprop */
                                       NULL,
                                       /* funcs of constructor, MyConstructor.myfunc() */
                                       NULL);
    if (prototype == NULL)
        gjs_fatal("Can't init class %s", constructor_name);

    g_assert(gjs_object_has_property(context, in_object, constructor_name));

    priv = g_slice_new0(Interface);
    priv->info = info;
    priv->gtype = g_registered_type_info_get_g_type(priv->info);
    g_base_info_ref((GIBaseInfo*)priv->info);
    JS_SetPrivate(context, prototype, priv);

    gjs_object_get_property(context, in_object, constructor_name, &value);

    if (!JSVAL_IS_OBJECT(value)) {
        gjs_throw(context, "Property '%s' does not look like a constructor",
                  constructor_name);
        return FALSE;
    }

    /* HACK HACK HACK HACK!
       We need the constructor to have a custom JS class to support
       hasInstance. But we also need another constructor defined by JS_InitClass, to
       keep prototype and have our resolve hook called.
       So we first create a dynamic class in the standard way, and then
       create an instance of our "interface constructor class" and link it
       to the actual prototype by setting "prototype" and "constructor".
       The real constructor for the interface is kept in the import global.
    */
    constructor = make_custom_constructor(context, prototype);
    value = OBJECT_TO_JSVAL(constructor);
    JS_SetProperty(context, in_object, constructor_name, &value);

    gjs_define_static_methods(context, constructor, priv->gtype, priv->info);

    value = OBJECT_TO_JSVAL(gjs_gtype_create_gtype_wrapper(context, priv->gtype));
    JS_DefineProperty(context, constructor, "$gtype", value,
                      NULL, NULL, JSPROP_PERMANENT);

    if (prototype_p)
        *prototype_p = prototype;

    return JS_TRUE;
}
