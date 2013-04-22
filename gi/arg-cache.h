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

#ifndef __GJS_ARG_CACHE_H__
#define __GJS_ARG_CACHE_H__

#include <glib.h>
#include <girepository.h>

#include "gjs/jsapi-util.h"
#include "gi/function.h"

G_BEGIN_DECLS

typedef struct _GjsArgumentCache {
    JSBool (*marshal_in) (JSContext                *context,
                          struct _GjsArgumentCache *cache,
                          GjsFunctionCallState     *state,
                          GArgument                *in_argument,
                          jsval                     value);
    JSBool (*marshal_out) (JSContext                *context,
                           struct _GjsArgumentCache *cache,
                           GjsFunctionCallState     *state,
                           GArgument                *out_argument,
                           jsval                    *value);
    JSBool (*release) (JSContext                *context,
                       struct _GjsArgumentCache *cache,
                       GjsFunctionCallState     *state,
                       GArgument                *in_argument,
                       GArgument                *out_argument);
    JSBool (*free) (struct _GjsArgumentCache *cache);

    const char *arg_name;
    int         arg_index;
    GITypeInfo  type_info;

    guint       skip_in : 1;
    guint       skip_out : 1;
    GITransfer  transfer : 2;
    guint       nullable : 1;
    guint       is_return : 1;

    union {
        /* for explicit array only */
        struct {
            int length_arg;
            GITypeTag length_tag;
        } array;

        struct {
            GIScopeType scope;
            int closure;
            int destroy;
        } callback;

        struct {
            GITypeTag number_tag;
            guint is_unsigned : 1;
        } number;

        /* boxed / union / GObject */
        struct {
            GType gtype;
            GIBaseInfo *info;
        } object;

        /* foreign structures */
        GIStructInfo *tmp_foreign_info;

        /* enum / flags */
        struct {
            gint64 enum_min;
            gint64 enum_max;
        } enum_type;
        guint64 flags_mask;

        /* string / filename */
        guint string_is_filename : 1;

        /* out caller allocates (FIXME: should be in object) */
        gsize caller_allocates_size;
    } contents;
} GjsArgumentCache;

JSBool gjs_arg_cache_build_arg (GjsArgumentCache *self,
                                GjsArgumentCache *arguments,
                                int               gi_index,
                                GIDirection       direction,
                                GIArgInfo        *arg,
                                GICallableInfo   *callable,
                                JSBool           *inc_counter);

JSBool gjs_arg_cache_build_return  (GjsArgumentCache *self,
                                    GjsArgumentCache *arguments,
                                    GICallableInfo   *info,
                                    JSBool           *inc_counter);

JSBool gjs_arg_cache_build_instance (GjsArgumentCache *self,
                                     GICallableInfo   *info);

static inline JSBool
gjs_arg_cache_is_skip_in (GjsArgumentCache *cache)
{
    return cache->skip_in;
}

static inline JSBool
gjs_arg_cache_is_skip_out (GjsArgumentCache *cache)
{
    return cache->skip_out;
}


G_END_DECLS

#endif  /* __GJS_ARG_CACHE_H__ */
