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
# Recursively, to_binary(6) expands to:

to_binary(6) = to_binary(3) + (6 % 2).to_s
to_binary(3) = to_binary(1) + (3 % 2).to_s
to_binary(1) = '1'

# Recursing back up and substituting terms in:

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

Fire up the REPL by typing in `./bits-repl` on terminal and let's get started!

### Bitshifting

The first operator we'll explore is bitshifting. There are actually two bitshifting operators, bit left shift `<<` and bit right shift `>>`.

* Bitwise Left Shift `<<`
    * In the REPL, type in `1 << 1` and press enter. What happened?
    * Now type in `8 << 2` and `4 << 4`. Where did the `1` go and how many places did it move by?
    * Given what you know so far from the REPL, what does `3 << 1` evaluate to in binary and in base 10? Try doing this on a piece of paper before hitting enter on the REPL.
    * Since we're manipulating 32 bit `int`s, let's try breaking things! What happens when you evaluate `1 << 32` in the REPL? Is this what you expected?

You don't have to answer this next question, but try entering in `1 << 31`. Were you surprised at all? We'll talk more about this later when we talk about how integers are represented.

For now, let's move onto bitwise right shift!

* Bitwise Right Shift `>>`
    * In the REPL, type in `2 >> 1` and press enter. What happened?
    * Now type in `32 >> 2`. Which direction did the `1` move and how many places did it move by?
    * Let's give `10 >> 3` a shot. Did that do what you expected it to?
    * What does `1 << 2 >> 3` evaluate to? Try to see if you can solve this before letting the REPL do its magic!

Feel free to explore more with the bitwise left shift and right shift operators before moving on to the next section!

### Bitwise &, | and ^

The second set of operators we'll be exploring are bitwise `and`, `or`, and `xor`.

* Bitwise And: `&`
    * To understand what `&` does, `&` all the different combinations of `0` and `1` (i.e `0 & 0`, `0 & 1`) and record your findings in the below table.
      I've filled out the upper left entry to help get you started which is the result of `0 & 0`.
    |               |   0   |   1   |
    | ------------- |-------|-------|
    |       0       |   0   |       |
    |       1       |       |       |
    * After filling out the table, try using `&` on other numbers! Try `31 & 30`. Do the bits follow your findings that you have in your table?

Write a Ruby method, `first_n_set` that takes in two integers, with the second integer representing how many bits in the first number should be set. 
It should return `true` if the first `n` bits of a number are set and `false` otherwise.

Assume the second integer is 0 or greater. Only use math and bitwise operators.

Example calls to `first_n_set` look like:

```ruby
first_n_set(7, 3)
# true because 7 in base 2 is 111 and n = 3 meaning we should check for the first 3 bits being set (which they are)

first_n_set(7, 1)
# true because we're only checking for the first bit and 7 in binary is '111'

first_n_set(8, 3)
# false since 8 in base 2 is 1000 and n = 3. We see that the first bit is `0`, meaning we should just return false
```

Here's a method signature to help you get started. Feel free to write in this markdown file (or create a separate one) so we can easily take a look:

```ruby
def first_n_set(num, n)
   # TODO: Bitshifting magic!
end
```

Just as an aside, the bitwise operators in Ruby work the same way as in the REPL, but feel free to use the REPL as a way to test out different scenarios! 

Hint: You'll need to use `>>` or `<<` in conjunction with `&`.

* Bitwise Or: `|`
    * Fill out the same table above but try it out with `|`
    |               |   0   |   1   |
    | ------------- |-------|-------|
    |       0       |       |       |
    |       1       |       |       |
    * Try to experiment with `|` with other numbers as well!

Write a Ruby method, `set_nth_bit` which takes two integers with the first being a integer and the second being `n` that returns that same number with the `nth` bit flipped on. 
If the bit is already turned on, return the original number (hint: you won't need to implement any special logic for this if you use `|` correctly).

Assume that the second parameter passed in is strictly greater than 0.

Example calls to `set_nth_bit` look like:

```ruby
set_nth_bit(8, 1)
# 9 because 8 in binary is 1000 and setting the first bit would flip the first 0 from the right on resulting in 1001 which is 9 in base 10.

set_nth_bit(8, 2)
# 10 because 8 in binary is 1000 and setting the second bit would flip the second 0 from the right on resulting in 1010 which is 10 in base 10.

set_nth_bit(8, 4)
# 8 because 8 in binary is 1000. Notice that the fourth bit from the right is already on, and so we'll just return 8.
```

Here's a method signature to help you get started:

```ruby
def set_nth_bit(num, n)
   # TODO: Bitshifting magic!
end
```

And again, feel free to submit PRs so we can take a look!

* Bitwise XOr: `^`
    * Here we go with the table again! This operator is a little trickier, but it should make sense after filling out this table:
    ^ | 0 | 1
    --- | --- | ---
    0 |   |
    1 |   |
    * After filling out the table, again, experiment with `^` before moving on to the problem below!

Write a Ruby method, `flip_nth_bit` which is similar to `set_nth_bit` except if the bit at the `nth` position is already set, the bit should be flipped back to 0.

Example calls to `flip_nth_bit` look like:

```ruby
flip_nth_bit(4, 3) 
# 0 because 4 in binary is 100 and the `nth` bit from the right in this case is turned on. So it should be turned off leaving us with 000 which is equal to 0. 

flip_nth_bit(4, 2) 
# 6 because 4 in binary is 100 and the `nth` bit from the right in this case is not turned on. So it should be turned on in this case leaving us with 110 which is 6 in base 10.

flip_nth_bit(0, 4)
# 16 because 0 in binary is 0000. The fourth bit from the right needs to be turned on. Once it's turned on, we get a binary representation of 1000 which is 16 in base 10.
```

Here's a method signature to help you get started. Again only use math and bitwise operators:

```ruby
def set_nth_bit(num, n)
   # TODO: Bitshifting magic!
end
```

Hint: This should be pretty similar to `set_nth_bit` but just with using a different operator.

### Last but `not` least... bitwise not!

Unlike the other operators, bitwise `not` is a unary operator meaning that it's an operation that works on a single number.

Thankfully, it's one of the easier operators to wrap your head around.

Try it out! See what `~42` does. Don't worry about the base 10 representation just yet (why is it a negative number? We'll find out!).

## Final Thoughts

Now that you understand how to manipulate bits, we can get into the more interesting applications about how to apply this knowledge and how it affects systems today!

Please feel free to email feedback at `justinharjanto@gmail.com` or post a new issue on this repository. Onwards!
