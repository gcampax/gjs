// application/javascript;version=1.8
// Copyright 2013 Giovanni Campagna
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.

const Lang = imports.lang;
const GObject = imports.gi.GObject;

let Gtk;

function _startsWith(string, substr) {
    if (string.substr(0, substr.length) == substr)
        return true;
    return false;
}

const GtkWidgetClass = new Lang.Class({
    Name: 'GtkWidgetClass',
    Extends: GObject.Class,

    _init: function(params) {
        let template = params.Template;
        delete params.Template;

        let children = params.Children;
        delete params.children;

        let internalChildren = params.InternalChildren;
        delete params.internalChildren;

        if (template) {
            params._instance_init = function() {
                this.init_template();
            };
        }

        this.parent(params);

        if (template) {
            if (typeof template == 'string' &&
                _startsWith(template, 'resource:///'))
                Gtk.Widget.set_template_from_resource.call(this, template);
            else
                Gtk.Widget.set_template.call(this, template);
        }

        this.Template = template;

        if (children) {
            for (let i = 0; i < children.length; i++)
                Gtk.Widget.automate_child.call(this, children[i], false, -1);
        }

        if (internalChildren) {
            for (let i = 0; i < internalChildren.length; i++)
                Gtk.Widget.automate_child.call(this, internalChildren[i], true, -1);
        }
    },

    _isValidClass: function(klass) {
        let proto = klass.prototype;

        if (!proto)
            return false;

        // If proto == Gtk.Widget.prototype, then
        // proto.__proto__ is GObject.InitiallyUnowned, so
        // "proto instanceof Gtk.Widget"
        // will return false.
        return proto == Gtk.Widget.prototype ||
            proto instanceof Gtk.Widget;
    },
});

function _init() {

    Gtk = this;

    Gtk.Widget.prototype.__metaclass__ = GtkWidgetClass;
}
