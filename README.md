# monet

A domain-specific language for prototyping business-logic and deal-flow contracts on [Filecoin Virtual Machine](https://fvm.filecoin.io/).

## spec

See [language spec](SPEC.md).

## examples

Atomic-swapping of Alice's FIL and Bob's wrapped FIL, escrow-style.

```lisp
When
    [Case
        (Deposit
            (Address "f1ginx5tebjahoqd4m5kdz3p5hqzhf54qhuwbf6ba")
            (Address "f1ginx5tebjahoqd4m5kdz3p5hqzhf54qhuwbf6ba")
            (Token "" "")  ; Empty name and tick means native FIL.
            (MulValue
                (Constant (1_000_000 + 0xaf))
                (ConstantParam "Amount of FIL")
            )
        )
        (When
            [Case
                (Deposit
                    (Address "f1ginx5tebzahoqd4m5kdz3p5hbzhf54qhuwbf6ba")
                    (Address "f1ginx5tebzahoqd4m5kdz3p5hbzhf54qhuwbf6ba")
                    (Token "wFIL" "Wrapped FIL")
                    (ConstantParam "Amount of wFIL")
                )
                (Pay
                    (Address "f1ginx5tebjahoqd4m5kdz3p5hqzhf54qhuwbf6ba")
                    (Address "f1ginx5tebzahoqd4m5kdz3p5hbzhf54qhuwbf6ba")
                    (Token "" "")
                    (MulValue
                        (Constant 1_000_000)
                        (ConstantParam "Amount of FIL")
                    )
                    (Pay
                        (Address "f1ginx5tebzahoqd4m5kdz3p5hbzhf54qhuwbf6ba")
                        (Address "f1ginx5tebjahoqd4m5kdz3p5hqzhf54qhuwbf6ba")
                        (Token "wFIL" "Wrapped FIL")
                        (ConstantParam "Amount of wFIL")
                        Close
                    )
                )
            ]
            (TimeParam "wFILmaturityDate")
            Close
        )
    ]
    (TimeParam "FILMaturityDate")
    Close
```

The contract is written in s-expressions defining what happen synchronously, and conditionally as events are satisfied.

