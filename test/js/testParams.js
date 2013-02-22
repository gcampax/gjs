// application/javascript;version=1.8

const Params = imports.params;

function testParse() {
    let one = Params.parse(null, { anInt: 42 });
    assertEquals(42, one.anInt);

    let two = Params.parse({ anInt: 16 },
                           { anInt: 42,
                             aString: 'foo' });
    assertEquals(16, two.anInt);
    assertEquals('foo', two.aString);

    assertRaises(function() {
        Params.parse({ aString: 'foo' },
                     { anInt: 42 });
    });
}

function testFill() {
    let one = Params.fill(null, { anInt: 42 });
    assertEquals(42, one.anInt);

    let two = Params.fill({ anInt: 16 },
                          { anInt: 42,
                            aString: 'foo' });
    assertEquals(16, two.anInt);
    assertEquals('foo', two.aString);

    let three = Params.fill({ anInt: 16,
                              aString: 'foo' },
                            { anInt: 42,
                              aBool: false });
    assertEquals(16, three.anInt);
    assertEquals('foo', three.aString);
    assertEquals(false, three.aBool);
}

function testFilter() {
    let one = Params.filter(null, { anInt: 42 });
    assertEquals(42, one.anInt);

    let twoFrom = { anInt: 16 };
    let two = Params.filter(twoFrom,
                            { anInt: 42,
                              aString: 'foo' });
    assertEquals(16, two.anInt);
    assertEquals('foo', two.aString);
    assertFalse('anInt' in twoFrom);

    let threeFrom = { anInt: 16,
                      aString: 'foo' };
    let three = Params.filter(threeFrom,
                              { anInt: 42,
                                aBool: false });
    assertEquals(16, three.anInt);
    assertEquals(false, three.aBool);
    assertFalse('aString' in three);
    assertTrue('aString' in threeFrom);
    assertFalse('anInt' in threeFrom);
}

gjstestRun();
