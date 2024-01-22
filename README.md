# Brzozowski Regular Expression Pattern Matcher

Welcome to the Scala Derivative Regular Expression Matcher, a powerful tool for pattern matching based on derivatives of regular expressions. This matcher allows you to efficiently search, edit, and analyze text patterns using the principles of regular expressions.

## Purpose

Regular expressions are a fundamental tool for pattern matching in text, offering versatility in various applications, such as data analysis and security breach detection in network traffic. This repository introduces a novel approach by leveraging derivatives of regular expressions to enhance matching capabilities.

## How It Works

The core concept revolves around taking the derivative of a regular expression with respect to a character, resulting in a new expression that matches strings with the character chopped off.

## Usage

Explore the matcher with examples like:


    size(ders(("a" * 50).toList, EVIL)) == 7
    simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next) == ONE
    simp(Iterator.iterate(ONE:Rexp)(r => ALT(r, r)).drop(20).next) == ONE
    val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
    matcher(EVIL, "a" * 1000000) == false
These examples demonstrate the efficiency and versatility of the Scala Derivative Regular Expression Matcher in various scenarios.

Feel free to experiment and integrate this tool into your projects for enhanced text pattern matching.
