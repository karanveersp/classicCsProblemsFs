namespace CsProblemsLib

open Utils.Fs

module Fibonacci =

    // Works but not efficient for values > 10
    let rec fib n =
        if n < 2 then
            n
        else
            fib (n - 1) + fib (n - 2)

    // Memoization is a technique in which you store the results of computational tasks when
    // they are completed so that when you need them again, you can look them up instead
    // of needing to compute them a second (or millionth) time.

    let memo =
        seq {
            (0, bigint (0))
            (1, bigint (1))
        }
        |> Dict.ofSeq

    let rec fibMemo (n: int) =
        if not (memo.ContainsKey(n)) then
            let value = fibMemo (n - 1) + fibMemo (n - 2)
            memo.Add(n, value) // memoization
            value
        else
            memo.Item(n)

    // Fibonacci with iterative loop. Any problem that can be solved recursively
    // can also be solved iteratively.
    let fibIter n =
        if n = 0 then
            bigint (n)
        else
            let mutable last = bigint 0
            let mutable next = bigint 1

            for _ in 1 .. (n - 1) do
                let prev = last
                last <- next
                next <- prev + next

            next

    // Generating sequence of fibonacci values
    let fibGen n =
        seq {
            yield bigint 0
            if n > 0 then yield bigint 1
            let mutable last = bigint 0
            let mutable next = bigint 1

            for _ in 1 .. (n - 1) do
                let prev = last
                last <- next
                next <- prev + next
                yield next
        }
