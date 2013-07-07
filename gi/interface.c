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
#include "repo.h"

#include <gjs/gjs-module.h>
#include <gjs/compat.h>
#include <gjs/runtime.h>
#include <util/log.h>

#include <girepository.h>

typedef struct {
    GIInterfaceInfo *info;
    GType gtype;
} Interface;

static struct JSClass gjs_interface_class;

GJS_DEFINE_PRIV_FROM_JS(Interface, gjs_interface_class)

GJS_NATIVE_CONSTRUCTOR_DEFINE_ABSTRACT(interface)

static void
interface_finalize(JSFreeOp *fop,
                   JSObject *obj)
{
    Interface *priv;

    priv = JS_GetPrivate(obj);

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
                      JSObject **obj,
                      jsid      *id,
                      unsigned   flags,
                      JSObject **objp)
{
    Interface *priv;
    char *name;
    JSBool ret = JS_FALSE;
    GIFunctionInfo *method_info;

    *objp = NULL;

    if (!gjs_get_string_id(context, *id, &name))
        return JS_TRUE;

    priv = JS_GetPrivate(*obj);

    if (priv == NULL)
        goto out;

    method_info = g_interface_info_find_method((GIInterfaceInfo*) priv->info, name);

    if (method_info != NULL) {
        if (gjs_define_function(context, *obj,
                                priv->gtype,
                                (GICallableInfo*)method_info) == NULL) {
            g_base_info_unref((GIBaseInfo*)method_info);
            goto out;
        }

        *objp = *obj;
        g_base_info_unref((GIBaseInfo*)method_info);
    }

    ret = JS_TRUE;

 out:
    g_free (name);
    return ret;
}

static struct JSClass gjs_interface_class = {
    "GObject_Interface",
    JSCLASS_HAS_PRIVATE |
    JSCLASS_NEW_RESOLVE,
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
    NULL, NULL, NULL
};

static JSPropertySpec gjs_interface_proto_props[] = {
    { NULL }
};

static JSFunctionSpec gjs_interface_proto_funcs[] = {
    { NULL }
};

JSBool
gjs_define_interface_class(JSContext       *context,
                           JSObject        *in_object,
                           GIInterfaceInfo *info)
{
    Interface *priv;
    const char *constructor_name;
    JSObject *constructor;
    JSObject *prototype;
    jsval value;

    constructor_name = g_base_info_get_name((GIBaseInfo*)info);

    if (!gjs_init_class_dynamic(context, in_object,
                                NULL,
                                g_base_info_get_namespace((GIBaseInfo*)info),
                                constructor_name,
                                &gjs_interface_class,
                                gjs_interface_constructor, 0,
                                /* props of prototype */
                                &gjs_interface_proto_props[0],
                                /* funcs of prototype */
                                &gjs_interface_proto_funcs[0],
                                /* props of constructor, MyConstructor.myprop */
                                NULL,
                                /* funcs of constructor, MyConstructor.myfunc() */
                                NULL,
                                &prototype,
                                &constructor)) {
        g_error("Can't init class %s", constructor_name);
    }

    GJS_INC_COUNTER(interface);
    priv = g_slice_new0(Interface);
    priv->info = info;
    priv->gtype = g_registered_type_info_get_g_type(priv->info);
    g_base_info_ref((GIBaseInfo*)priv->info);
    JS_SetPrivate(prototype, priv);

    gjs_define_static_methods(context, constructor, priv->gtype, priv->info);

    value = OBJECT_TO_JSVAL(gjs_gtype_create_gtype_wrapper(context, priv->gtype));
    JS_DefineProperty(context, constructor, "$gtype", value,
                      NULL, NULL, JSPROP_PERMANENT);

    return JS_TRUE;
}

JSBool
gjs_lookup_interface_constructor(JSContext *context,
                                 GType      gtype,
                                 jsval     *value_p)
{
    JSObject *constructor;
    GIBaseInfo *interface_info;

    interface_info = g_irepository_find_by_gtype(NULL, gtype);

    if (interface_info == NULL) {
        gjs_throw(context, "Cannot expose non introspectable interface %s",
                  g_type_name(gtype));
        return JS_FALSE;
    }

    g_assert(g_base_info_get_type(interface_info) ==
             GI_INFO_TYPE_INTERFACE);

    constructor = gjs_lookup_generic_constructor(context, interface_info);
    if (G_UNLIKELY (constructor == NULL))
        return JS_FALSE;

    g_base_info_unref(interface_info);

    *value_p = OBJECT_TO_JSVAL(constructor);
    return JS_TRUE;
}
