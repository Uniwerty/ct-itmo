# Kotlin Array Declaration Parser

## Terminals:

- `var` – array variable declaration
- `n` – array or type name
- `:` – array name and type separator
- `Array` – array type
- `<`, `>` – array type parentheses
- `?` – type nullability indicator

## Non-Terminals:

- `S` – array declaration
- `A` – array type declaration
- `T` – type name
- `T'` – type parameterization
- `N` – type nullability

## Grammar:

- `S -> var n : A`
- `A -> Array < T > N`
- `T -> n T' N`
- `T -> A`
- `T' -> < T >`
- `T' -> ' '`
- `N -> ?`
- `N -> ''`

## FIRST and FOLLOW table

|      |    FIRST     |  FOLLOW  |
|------|:------------:|:--------:|
| `S`  |    `var`     |   `$`    |
| `A`  |   `Array`    | `$`, `>` |
| `N`  |  `?`, `''`   | `$`, `>` |
| `T`  | `n`, `Array` |   `>`    |
| `T'` |  `' '`, `<`  | `?`, `>` |
