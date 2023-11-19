{-# LANGUAGE OverloadedStrings #-}
import Private.Material
import Text.Parsec

main :: IO ()
main = do
    parseTest parseMaterialName "newmtl Material"
    parseTest parseNs "Ns 250.000000"
    parseTest parseKa "Ka 1.000000 1.000000 1.000000"
    parseTest parseMapKa "map_Ka -s 1.000000 1.000000 1.000000 somefile.tga"
    parseTest parseIllum "illum 4"
    parseTest comment "# Something is cooking"
    parseTest parseTf "Tf 1.000000 1.000000 1.000000"
    parseTest parseTf "Tf xyz 1.0 0.1 1.0" 
    parseTest parseTf "Tf spectral someFile.rfl"
    parseTest parseTf "Tf spectral someFile.rfl 3.0"

    val1 <- parseMtlFromFile "assets/material.mtl"
    print val1

    val2 <- parseMtlFromFile "assets/Example1.mtl"
    print val2

