Practical Applications of Bits and bytes
=======================================

In this card, we'll explore how we as software engineers can exploit how the computer interprets bits and bytes in order to make memory intensive applications to be *very* efficient.

We'll take a look at how a specialized version of hash sets called a *bit set* can reduce the amount of memory that's allocated. If you need a quick refresher on what a hash set is and how it's implemented, take a look [here](https://beginnersbook.com/java-tutorial-for-beginners-with-examples/).

So what's a bit set?
====================

A bit set is a special type of set that uses an array of bytes as its underlying backing data structure. This implementation is much more specific than how a hash set is implemented since usually, hash sets are implemented with an array of nodes where each node contains the key and value. Bit sets can only remember if they've seen an element. They cannot remember certain key value pairs and do not handle collisions.

This storing of not only the node, but also the key and value works well in most cases, but when the hash set grows to hold millions of elements, a given application might start to run low on memory to give to other parts of the application. To address this, we'll take advantage of the fact that each byte has 8 bits and we can use each bit to denote whether or not we've *seen* an element.

Sounds a little confusing? Let's go over the operations in Java!

Bit Set Operations
==================

```java
BitSet bitSet = new BitSet(16);
```

In the above line, we'll initialize a new `BitSet` with 16 spots. Since we have a request for 16 spots, we know we need exactly 2 bytes for a total of 16 bits.

Conceptually, the below diagram shows what it should look like:

```java
     BYTE 1             BYTE 2
-------------------------------------
| 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 |
-------------------------------------
```

In the `BitSet`'s initial state, all the bits are turned off.

We can perform operations such as `flip` to flip a given index from a 0 to 1 in the bit set:

```java
bitSet.flip(4); // Set the 5th bit in the first byte to be on (zero indexing!).

     BYTE 1             BYTE 2
-------------------------------------
| 0 0 0 0 1 0 0 0 | 0 0 0 0 0 0 0 0 |
-------------------------------------
```

For now, don't worry if a client calls `flip` with an invalid index such as `bitSet.flip(50)` in this example. We'll be addressing those edge cases later. For now let's try to understand what the available operations to us as clients to this `BitSet` are..

Conversly, performing the `flip` operation again will flip the bit in the `BitSet` off:

```java
bitSet.flip(4); // Set the 5th bit in the first byte to be off (zero indexing!).

     BYTE 1             BYTE 2
-------------------------------------
| 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 |
-------------------------------------
```

We can also call `contains` and pass an index to check if it's on:

```java
boolean fourFlippedOn = bitSet.contains(4); // Returns false
bitSet.flip(5);

     BYTE 1             BYTE 2
-------------------------------------
| 0 0 0 0 0 1 0 0 | 0 0 0 0 0 0 0 0 |
-------------------------------------

boolean fiveFlippedOn = bitSet.contains(5); // Returns true
```

The last useful operation is `clear` which will zero out all of the turned on bits in the `BitSet`:

```java
bitSet.flip(9);
bitSet.flip(10);
bitSet.flip(11);

     BYTE 1             BYTE 2
-------------------------------------
| 0 0 0 0 0 1 0 0 | 0 1 1 1 0 0 0 0 |
-------------------------------------

bitSet.clear();

     BYTE 1             BYTE 2
-------------------------------------
| 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 |
-------------------------------------
```

Building a BitSet
==================

Using what we've learned about bitwise operations from the first training module, let's go ahead and try implementing our own BitSet and use it to solve a tough problem!

We'll be using `Java` to implement a `BitSet`. For a quick refresher on basic `Java` syntax, check out this page. All the bitwise operators like `<<`, `>>` you saw in the `REPL` from the previous card will work just as expected in Java.

When you're ready to start, feel free to open up `src/BitSet.java` to begin!
