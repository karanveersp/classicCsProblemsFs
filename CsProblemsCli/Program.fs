open CsProblemsLib.GeneCompression

open System
open System.Runtime.InteropServices

let geneCompressionTest () =
    let s =
        String.replicate 100 "TAGGGATTAACCGTTATATATATATAGCCATGGATCGATTATATAGGGATTAACCGTTATATATATATAGCCATGGATCGATTATA"

    let compressed = new CompressedGene(s)

    printfn $"Original is {System.Text.ASCIIEncoding.ASCII.GetByteCount(s)} bytes"
    printfn $"Compressed is {compressed.BitString.GetByteCount()} bytes"

    printfn $"Original and decompressed are the same: {s = compressed.Decompress()}"

geneCompressionTest ()
