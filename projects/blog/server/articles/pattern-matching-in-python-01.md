# Паттерн матчинг в Python

Последняя версия Python (3.10), выпущенная около года назад, привнесла в язык новый синтаксический конструкт, известный как Structural Pattern Matching. На первый взгляд он кажется непонятным и излишним: тех же результатов можно добиться, используя знакомый всем `if/else`. В этой статье разберем мотивацию для добавления паттерн матчинга и особенности его реализации в Python в сравнении с другими языками.

## Анатомия паттерн матчинга

Для начала краткое введение для тех, кто ещё не знаком с паттерн матчингом в Python. Новый синтаксис выглядит так:

```python
match subject:
    case pattern0:
        action_0()
    case pattern1:
        action_1()
    case _:
        fallback_action()
```

Результат выполнения выражения после ключевого слова `match` называется **subject**. Субъект сравнивается с паттернами, приведенными после слов `case`. Когда первый подходящий паттерн найден, выполняется код внутри соответствующего блока `case`. В этом блоке также будут доступны дополнительные переменные, образовавшиеся в результате **«**разложения**»** субъекта в соответствии с паттерном. Выполнение `match` заканчивается при первом совпадении, т.е. если субъект можно разложить и по `pattern0`, и по `pattern1`, то выполнится только код в `case` блоке с `pattern0`, аналогично блокам `if/elif/elif/…`. Паттерн `case _` выполняет роль `else`, т.е. будет выполнен, если предыдущие паттерны не сработали. Так же как и блок `else`, `case _` является необязательным.

Допустим, мы хотим написать игру, в которой пользователь должен вводить команды в текстовом виде, такие как `подпрыгни`, `купи меч`, `купи хлеб сыр`, `иди в магазин`, `выйди из магазина`, `выйди из игры`. Реализация с использованием паттерн матчинга может выглядеть так:

```python
command = input("Что сделать дальше? ")
match command.split():
    case ["подпрыгни"]:
        jump()
    case ["иди", "в", destination]:
        go(destination)
    case ["купи", *items]:
        for item in items:
            buy(item)
    case ["выйди", "из", "игры"]:
        print("Игра закрывается...")
        quit_game()
    case ["выйди", "из", place]:
        leave(place)
    case _:
        raise TypeError("Команда не найдена")
```

Очевидно, что реализация подобного механизма на `if/elif/else` будет менее читабельной. Паттерн матчинг может работать не только со списками, но и со словарями, примитивными значениями, экземплярами классов, он может содержать дополнительные проверки, может быть вложенным. В этой статье мы не будем разбирать все возможности паттерн матчинга, а лишь попытаемся понять, зачем его добавили и что вдохновило разработчиков Python на добавление нового синтаксиса. Но ещё один пример я приведу для наглядности:

```python
def fibonacci(a):
    match a:
        # Дополнительная проверка с if, называемая "guard"
        case n if n < 0:
            raise ValueError("Аргумент должен быть неотрицательным")
        case 0:
            return 0
        # | означает "или"
        case 1 | 2:
            return 1
        # Паттерн сработает только для целых чисел
        case int(a):
            return fibonacci(a - 1) + fibonacci(a - 2)
        case _:
            raise ValueError(f"Значение {a} не поддерживается")
```

## Мотивация

> Синтаксис для паттер матчинга можно найти во многих языках, от Haskell, Erlang и Scala до Elixir и Ruby. (Предложение для JavaScript тоже на рассмотрении.) — PEP 635

Паттерн матчинг широко распространен в функциональных языках программирования, таких как Haskell, Scala, ML. Программирование на них невозможно представить без паттерн матчинга. Python — мультипарадигменный язык. В нём, помимо ООП, реализована поддержка идей из функциональных языков. Это глобальный тренд — добавлять в ОО языки возможности функциональных собратьев, а также использовать подходы и библиотеки, позволяющие писать код в более функциональном стиле (иммутабельность, чистые функции).

## Особенности паттерн матчинга в Python в сравнении с другими языками

### В Python результат паттерн матчинга нельзя присвоить переменной

Одним из принципиальных отличий `match` в Python от аналогичных конструкций в функциональных языках является тот факт, в Python `match` — это оператор (statement), а не выражение (expression). На практике это означает, что мы не можем присвоить результат паттерн матчинга в Python какой-то переменной.

В Rust конструкция `match` является выражением, поэтому подобный код компилируется без ошибок:

```rust
enum Color {
  Red,
  White,
  Black,
}

fn main() {
  let color = Color::Red;
  // pattern matching here
  let cost = match color {
    Color::Red | Color::Black => 10,
    Color::White => 0,
  };
  println!("Cost is {}", cost);
}
```

Аналогично в Haskell:

```haskell
data Color = Red | White | Black

main :: IO ()
main =
    let
        color = Red
        -- pattern matching here
        cost  = case color of
                    Red   -> 10
                    Black -> 10
                    White -> 0
    in putStrLn $ "Cost is " ++ show cost
```

В Python такое невозможно.

### Паттерн матчинг в Python неисчерпывающий

Если мы удалим кейс с одним из цветов, Rust не позволит нам скомпилировать такой код:

```rust
enum Color {
  Red,
  White,
  Black,
}

fn main() {
  let color = Color::Red;
  let cost = match color {
    // Не хватает случая для Color::Black
    // Код не скомпилируется
    Color::Red => 10,
    Color::White => 0,
  };
  println!("Cost is {}", cost);
}
// Получим ошибку:
// pattern Black not covered
```

Haskell по умолчанию не выбросит ошибку во время компиляции, и поэтому может оказаться, что программа упадёт с исключением во время работы. Однако у компилятора существует флаг, который можно включить, и тогда Haskell выдаст предупреждение во время компиляции.

```haskell
-- Включена проверка на неполные паттерны
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Color = Red | White | Black

main :: IO ()
main =
    let
        color = Red
        -- Пропущен кейс с White
        cost  = case color of
                    Red   -> 10
                    Black -> 10
    in putStrLn $ "Cost is " ++ show cost

-- Результат:
-- warning: [-Wincomplete-patterns]
-- Pattern match(es) are non-exhaustive
-- In a case alternative: Patterns not matched: White
```

В Standard ML:

```ocaml
datatype Color = Red | White | Black

val color = Red

(* Пропущен кейс с Black *)
val cost = case color of
               Red => 10
             | White => 0

val () = print ("Cost is " ^ Int.toString cost ^ "\n")

(* Warning: match nonexhaustive *)
```

Python не проводит подобные проверки.

### Динамическая типизация

В Python можно смешивать различные типы в одном `match` операторе:

```python
x = 1
match x:
    case "1":
        print("x is string")
    case [1]:
        print("x is a list")
    case {}:
        print("x is a dict")
    case 1:
        print("x is a number")

```

Лично для меня это выглядит немного дико. Я привык к строгой статической типизации. Чтобы сделать код на Python более надежным, можно использовать статические анализаторы кода для проверки типов, такие как [Pyright](https://github.com/microsoft/pyright). Они способны проверять, что все возможные кейсы в `match` обработаны, иначе выбрасывать ошибки:

```python
# pyright: strict

from enum import Enum, auto

class Color(Enum):
    RED = auto()
    WHITE = auto()
    BLACK = auto()

def get_cost(color: Color) -> int:
    # Pyright error: Unhandled type: "Literal[Color.WHITE]"
    match color:
        case Color.RED:
            return 10
        case Color.BLACK:
            return 10

def main():
    color = Color.RED
    cost = get_cost(color)
    print(f"Cost is {cost}")

if __name__ == "__main__":
    main()
```

## Вывод

Паттерн матчинг в Python — это удобный синтаксический сахар в некоторых ситуациях. Он был вдохновлен подобными конструкциями других языков программирования, прежде всего функциональных. Наибольшую пользу паттерн матчинг приносит именно в языках со статической типизацией: в них компилятор может отловить ошибки, когда не все возможные паттерны были учтены. В Python можно использовать статические анализаторы кода для проверки `match` операторов на исчерпывающую обработку всех возможных кейсов.

Что ещё почитать:

1. [PEP 636](https://peps.python.org/pep-0636/). Structural Pattern Matching: Tutorial
2. [PEP 635](https://peps.python.org/pep-0635/). Structural Pattern Matching: Motivation and Rationale
3. [PEP 634](https://peps.python.org/pep-0634/). Structural Pattern Matching: Specification
4. [Pyright](https://github.com/microsoft/pyright). Static type checker for Python
