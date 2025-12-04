# Logic Checker

## Introduction
This is logic checker written in Haskell.

I applied two ways to check the logic.

1. Base on Venn plot
2. Base on Algebraic.

## Usage
```
./logic-hs
```

You can input the statement you want to validate.
it will looks like this
```
zhanglab@hpc37new:~/example/logic_hs$ ./logic-hs 
Logical argument checker (Venn + Algebraic)

Enter an argument to be checked.
Format per line: Quantifier Subject Predicate
  Quantifier: Every | Some | No | SomeNot
  Subject, Predicate: A | B | C
Example premise: Every A B
Enter premises first, then a blank line, then the conclusion.
Premise 1 (blank to finish): Every A B
Premise 2 (blank to finish): SomeNot A C
Premise 3 (blank to finish): 
Conclusion: SomeNot B C
=========================================
Argument:
  Every A B
  SomeNot A C
  Therefore: SomeNot B C
-----------------------------------------
=== Venn / World-Model Checker ===
Premises:   [Every A B,SomeNot A C]
Conclusion: SomeNot B C
Verdict:    True - Valid

=== Algebraic / Barbara-Triad Checker ===
Premises:   [All a are b,Some a is not c]
Conclusion: Some b is not c
Verdict:    True - Valid! (Reduces to Barbara Triad)
=========================================
Check another argument? (y/n): n
Goodbye.
```