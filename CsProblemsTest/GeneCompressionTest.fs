module CsProblemsTest

open NUnit.Framework
open CsProblemsLib.GeneCompression

[<SetUp>]
let Setup () = ()

[<Test>]
let GeneCompressionProducesExpectedBitString () =
    let c = new CompressedGene("ACGT")
    let expected = bigint 0b100011011
    Assert.AreEqual(expected, c.BitString)

[<Test>]
let BitsDecompressToOriginalString () =
    let original = "ACGT"
    let c = new CompressedGene(original)
    let decompressed = c.Decompress()
    Assert.AreEqual(original, decompressed)
