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
    JSBool (*marshal) (struct _GjsArgumentCache *, GArgument *, jsval *);
    JSBool (*release) (struct _GjsArgumentCache *, GArgument *);
    JSBool (*free) (struct _GjsArgumentCache *);

    /* For compatibility */
    GjsParamType param_type;

    union {
        gint dummy;
    } contents;
} GjsArgumentCache;

JSBool gjs_arg_cache_build_in_arg (GjsArgumentCache *self,
                                   GjsParamType     *param_types,
                                   int               gi_index,
                                   GIArgInfo        *arg,
                                   JSBool           *inc_counter);

JSBool gjs_arg_cache_build_out_arg (GjsArgumentCache *self,
                                    GjsParamType     *param_types,
                                    int               gi_index,
                                    GIArgInfo        *arg,
                                    JSBool           *inc_counter);

JSBool gjs_arg_cache_build_inout_arg (GjsArgumentCache *in_self,
                                      GjsArgumentCache *out_self,
                                      GjsParamType     *param_types,
                                      int               gi_index,
                                      GIArgInfo        *arg,
                                      JSBool           *inc_counter);

JSBool gjs_arg_cache_build_return  (GjsArgumentCache *self,
                                    GjsParamType     *param_types,
                                    GICallableInfo   *info,
                                    JSBool           *inc_counter);

G_END_DECLS

#endif  /* __GJS_ARG_CACHE_H__ */
