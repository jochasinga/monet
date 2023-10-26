# monet (wip)

DSL for prototyping deal flow actors on FVM.

## example

Atomic-swapping of Alice's FIL and Bob's wFIL.

```lisp
When
    [Case 
        (Deposit
            (Role "alice")
            (Role "alice")
            (Token "" "")
            (MulValue
                (Constant 1000)
                (ConstantParam "Amount of FIL")
            )
        )
        (When
            [Case
                (Deposit
                    (Role "bob")
                    (Role "bob")
                    (Token "wFIL" "Wrapped FIL")
                    (ConstantParam "Amount of wFIL")
                )
                (Pay
                    (Role "alice")
                    (Party (Role "bob"))
                    (Token "" "")
                    (MulValue
                        (Constant 1000)
                        (ConstantParam "Amount of FIL")
                    )
                    (Pay
                        (Role "bob")
                        (Role "alice")
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