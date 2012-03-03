// application/javascript;version=1.8 -*- mode: js; indent-tabs-mode: nil -*-

if (!('assertEquals' in this)) { /* allow running this test standalone */
    imports.lang.copyPublicProperties(imports.jsUnit, this);
    gjstestRun = function() { return imports.jsUnit.gjstestRun(window); };
}

const Lang = imports.lang;
const GObject = imports.gi.GObject;
const Gio = imports.gi.Gio;

function testInterfaceStatic() {
    // The object exists
    assertNotUndefined(Gio.Icon);

    // Static methods are present
    assertEquals('function', typeof Gio.Icon.new_for_string);

    // The GType is exposed (crucial to the duck-typing system)
    assertEquals(GObject.type_from_name('GIcon'), Gio.Icon.$gtype);
}

function testInterfaceConstructor() {
    let obj = Gio.Icon.new_for_string('gtk-yes');
    assertEquals('object', typeof obj);

    // Non static methods are present are callable
    assertEquals('function', typeof Gio.Icon.prototype.to_string);
    assertEquals('gtk-yes', Gio.Icon.prototype.to_string.call(obj));

    // obj has the right concrete class, but it's still
    // recognized by the interface
    assertTrue(obj instanceof Gio.ThemedIcon);
    assertTrue(obj instanceof Gio.Icon);
}

function testInterfaceConcrete() {
    let obj = new Gio.ThemedIcon({ name: 'gtk-yes' });
    assertEquals('object', typeof obj);

    assertTrue(obj instanceof Gio.ThemedIcon);
    assertTrue(obj instanceof Gio.Icon);

    assertEquals('gtk-yes', Gio.Icon.prototype.to_string.call(obj));
    assertEquals('gtk-yes', obj.to_string());
}

gjstestRun();
