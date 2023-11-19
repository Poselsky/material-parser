# Material Parser

This is simple, albeit verbose, parser which 
parsers materials paired to obj files.

[Material specification](https://paulbourke.net/dataformats/mtl/)
[Material specification wikipedia](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
[Material specification gov](https://www.loc.gov/preservation/digital/formats/fdd/fdd000508.shtml#notes)

```haskell
parseMtlFromFile -- input file
parseMtl         -- input text
```

## Maintenance

If you see something missing or a bug, fill in an issue.
If you need to add new field which I could have missed,
update these parts:

```haskell
-- new token field
data Token    
-- add field to final material
data Material 
-- initialMaterialState 
initMaterial  
-- all possible values within mtl
expr          
-- add pattern match of your field
writeTokenToMaterial 
```

## License: *This project falls under MIT license.*