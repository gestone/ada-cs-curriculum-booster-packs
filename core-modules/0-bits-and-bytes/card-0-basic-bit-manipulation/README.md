# Understanding Primitive Types & Basic Bit Manipulation

In this card, we will get familiar with basic bitwise operators and how to use them properly.

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


