namespace CsProblemsLib

module Extensions =
    open System
    open System.Globalization

    type String with
        member s.Reverse() =
            StringInfo.ParseCombiningCharacters(s)
            |> Array.rev
            |> Seq.map (fun i -> StringInfo.GetNextTextElement(s, i))
            |> String.concat ""


module GeneCompression =
    open System
    open Extensions

    let numBits (n: bigint) = n.GetBitLength()


    type CompressedGene(gene: string) as self =
        [<DefaultValue>]
        val mutable private bitString: bigint

        do self.compress gene

        member self.BitString: bigint = self.bitString

        member private _.displayBitString(bits: int) = printfn "%B" bits

        member private self.compress(gene: string) =
            self.bitString <- bigint 1
            printfn "Length of string: %d" gene.Length

            for nucleotide in gene.ToUpper() do
                // shift left 2
                self.bitString <- self.bitString <<< 2

                match nucleotide with
                | 'A' -> self.bitString <- self.bitString ||| (bigint 0b00)
                | 'C' -> self.bitString <- self.bitString ||| (bigint 0b01)
                | 'G' -> self.bitString <- self.bitString ||| (bigint 0b10)
                | 'T' -> self.bitString <- self.bitString ||| (bigint 0b11)
                | _ -> raise (Exception($"Invalid nucleotide: {nucleotide}"))

        member self.Decompress() : string =
            let mutable gene = ""

            // The first subtraction is to get rigt of placeholder bit
            // that we started compression from. The second subtraction
            // is for non-inclusive iteration.
            let mutable i = int64 0

            while (i < (self.bitString.GetBitLength() - 2L)) do
                // Get only 2 rightmost bits
                let bits = int (self.bitString >>> (int i) &&& (bigint 0b11))

                match bits with
                | 0b00 -> gene <- gene + "A"
                | 0b01 -> gene <- gene + "C"
                | 0b10 -> gene <- gene + "G"
                | 0b11 -> gene <- gene + "T"
                | _ -> raise (Exception($"Invalid bits: {bits}"))

                i <- i + 2L

            gene.Reverse()

        with
            override self.ToString() = self.Decompress()


// let display (bits: uint64) = printfn "%B" bits

// let x = 0b0001
// let y = x <<< 3

// display (x <<< 2)
// printfn $"{0b011 <<< 1 = 0b110}"

// // Bitwise AND with &&&
// printfn "%b" ((0b110 &&& 0b100) = 0b100)
// // Bitwise OR with |||
// printfn "%b" ((0b110 ||| 0b100) = 0b110)

// display 0b100
