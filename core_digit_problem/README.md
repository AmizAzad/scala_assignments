A core digit is defined as recursive summing of digits of a number until you are left with a single digit number.

e.g. Let us find the core digit of the number: 9785

9 + 7 + 8 + 5 = 29     -> Two digits, so sum again

2 + 9 = 11                 -> Two digits, so sum again

1 + 1 = 2               -> Single digit - so this is the core digit

Now the problem: you are given a string of 2 numbers separated by a space. E.g 9785  4

You need to construct a new number from the above by repeating the first as many times as the second number. So in the above example, the new number will be:

9785978597859785    -> (9785 repeated 4 times)

Now you will need to calculate the CoreDigit for this new number according to the above logic.

Limits: In the pair of numbers given above, the first number can be 100000 digits long (10 ^ 100000) and the second number can be 5 digits long.

Sample file with the two numbers attached.