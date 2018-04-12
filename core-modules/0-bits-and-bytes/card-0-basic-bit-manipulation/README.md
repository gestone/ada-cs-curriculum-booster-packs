# Basic Bit Manipulation & Primitive Types

In this card, we will get familiar with basic bitwise operators and how to use them properly.

We'll also relate binary to primitive types to get a sense of the importance of binary.

But first, before we jump right into it, let's have a little refresher on binary representation first! 

## Quick Binary Representation Review

When we talk about numbers, we're usually referring to numbers in base 10 where each digit in a position of a number can have a value from 0 to 9 (for a total of 10 different possibilites).
However, computers don't fair well with base 10, but instead like using base 2 where each number in the representation is either a 0 or a 1. We often just refer to this as binary.

As a quick example, let's do some conversions from base 10 to binary and back again from binary to base 10!

### Converting Base 10 to Binary

As a refresher, the steps for converting any base 10 to binary is as follows:

1. If the number is 0 or 1, we're done. 0 in binary is 0, and 1 in binary is 1.
2. Otherwise, take the current number and mod it by 2. This will be tacked on at the end of the binary number. It will be either a 0 or 1.
3. Now take the current number and divide it by 2 and repeat steps 1 - 3 again.

The above steps can be concisely be represented as a recursive function:

```ruby
def to_binary(num)
    if num == 0 or num == 1 # Our base case
        return num.to_s
    end
    return to_binary(num / 2) + (num % 2).to_s
end
```

Note that the above only works for positive numbers. We'll address how we represent negative numbers in this lab.

Using the above definition, let's start off with converting the number `6` to binary.

```ruby
Recursively, to_binary(6) expands to:

to_binary(6) = to_binary(3) + (6 % 2).to_s
to_binary(3) = to_binary(1) + (3 % 2).to_s
to_binary(1) = '1'

Recursing back up and substituting terms in:

to_binary(3) = '1' + (3 % 2).to_s
to_binary(3) = '1' + '1'
to_binary(3) = '11'

to_binary(6) = to_binary(3) + (6 % 2).to_s
to_binary(6) = '11' + (6 % 2).to_s
to_binary(6) = '11' + '0'
to_binary(6) = '110'
```

And we're done!

### Convert Binary to back to Base 10

How do we know we're right? Let's go the other way!

From `110`, multiply each digit by the `2 ^ ith` position where the rightmost digit starts off at 0 and each digit moving leftward is 1 higher than the last.

This will make more sense with an example. Converting `110` into base 10 starting from the left 0:

```ruby
0 * (2 ^ 0) + 1 * (2 ^ 1) + 1 * (2 ^ 2)
     0      +     2       +     4        =  6
```

Great! If you need more practice, remember to take a look at the pre-reading to get a firmer grasp on binary and base 10.

## A quick detour toward the land of primitives

Throughout your time programming, you may have heard of terms like `int`, `long`, `float` and `double`. What's the difference between all of them?

An `int` is a natural number represented with 32 bits. Put another way, `int`s are represented with 4 bytes as a single byte is represented with 8 bits.
A `long` is a larger integer data type and is represented with 64 bits or 8 bytes.

And finally, `float` is a decimal number represented with 32 bits and `double` is also a decimal but represented with 64 bits instead.

Note that because each of these data types have a finite number of bits, we cannot represent arbitrarly large numbers. For example, for `int`s, we can only represent at max 2 ^ 32 possible values (0 or 1 for each of the bits).

We'll explore more later about how the specifics of each of these data types work!

## Bitwise Operators

Rather than spilling the beans and go over what each bitwise operator does in depth, we'll be using a special REPL specifically designed to help explore the various bitshifting operators.

In this REPL, we'll be manipulating 32 bit integers, so fractional values won't work with the REPL.

Answer the following questions right on this README.md and submit a pull request to this repository so we can take a look!

Fire up the REPL by invoking `./bits-repl` and let's get started!

### Bitshifting

The first operator we'll explore is bitshifting. There are actually two bitshifting operators, bit left shift `<<` and bit right shift `>>`.

1. Bitwise Left Shift `<<`
    a. In the REPL, type in `1 << 1` and press enter. What happened?
    b. Now type in `2 << 2` and `4 << 4`. Where did the `1` go and how many places did it move by?
    c. Given what you know so far from the REPL, what does `3 << 1` evaluate to in binary and in base 10? Try doing this on a piece of paper before hitting enter on the REPL.
    d. Since we're manipulating 32 bit `int`s, let's try breaking things! What happens when you evaluate `1 << 32` in the REPL? Is this what you expected?

You don't have to answer this next question, but try entering in `1 << 31`. Were you surprised at all? We'll talk more about this later when we talk about how integers are represented.

For now, let's move onto bitwise right shift!

2. Bitwise Right Shift `>>`
    a. In the REPL, type in `2 >> 1` and press enter. What happened?
    b. Now type in `32 >> 2`. Which direction did the `1` move and how many places did it move by?
    c. Let's give `10 >> 3` a shot. Did that do what you expected it to?
    d. What does `1 << 2 >> 3` evaluate to? Try to see if you can solve this before letting the REPL do its magic!

Feel free to explore more with the bitwise left shift and right shift operators before moving on to the next section!


