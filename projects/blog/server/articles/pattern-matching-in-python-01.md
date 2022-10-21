# Обзор паттерн матчинга в Python

![Tinder match between Python and Haskell](/assets/images/pattern-matching-wide.jpg)

Последняя версия Python (3.10), выпущенная около года назад, привнесла в язык новый синтаксический конструкт, известный как Structural Pattern Matching. На первый взгляд он кажется непонятным и излишним: тех же результатов можно добиться, используя знакомый всем `if/else`. В этой статье разберем паттерн матчинг в Python: мотивацию для его добавления, синтаксис и особенности работы.

## Мотивация

> Синтаксис для паттер матчинга можно найти во многих языках, от Haskell, Erlang и Scala до Elixir и Ruby. (Предложение для JavaScript тоже на рассмотрении.) — PEP 635

Так выглядит паттерн матчинг в Haskell:

```haskell
-- Описание типа функции:
-- Тип `a` должен быть сравниваемым (`Eq`)
-- Функция `inList` принимает объект типа `a`
-- И список объектов типа `a` (`[a]`)
-- Возвращает `Bool`
inList :: (Eq a) => a -> [a] -> Bool
inList a list =
    -- Паттерн матчинг с помощью `case` выражения
    case list of
        -- Первый паттерн: когда список пустой, вернуть `False`
        [] -> False

        -- Второй паттерн: разбиваем список на первый член `x` и список остальных членов `xs`
        (x:xs) ->
            if a == x then True
            else inList a xs

-- Паттерн матчинг в объявлении функции
inList :: (Eq a) => a -> [a] -> Bool
-- Первый паттерн: когда список пустой, вернуть `False`
inList _ [] = False  
-- Второй паттерн: разбиваем список на первый член `x` и список остальных членов `xs`
inList a (x:xs)
    if a == x then True
    else inList a xs
```

Аналогичная реализация в Python:

```python
# Без паттерн матчинга
def in_list(a, l):
    if not isinstance(l, list):
        raise TypeError('Expected a list as the second argument')
    if len(l) == 0:
        return False
    else:
        x = l[0]
        xs = l[1:]
        return True if x == a else in_list(a, xs)

# С паттерн матчингом
def in_list(a, l):
    match l:
      case []:
        return False
      case [x, *xs]:
        return True if x == a else in_list(a, xs)
      case _:
        raise TypeError('Expected a list as the second argument')
```

С паттерн матчингом упрощается проверка на соответствие переданных типов ожидаемым, а для более сложных случаев ещё и повышается читабельность кода.

## Анатомия паттерн матчинга

Новый синтаксис выглядит так:

```python
match subject:
    case pattern0:
        action_0()
    case pattern1:
        action_1()
    case _:
        fallback_action()
```

Результат выполнения выражения после ключевого слова `match` называется **subject**. Субъект сравнивается с паттернами, приведёнными после слов `case`. Когда первый подходящий паттерн найден, выполняется код внутри соответствующего блока `case`. В этом блоке также будут доступны дополнительные переменные, образовавшиеся в результате «разложения» субъекта в соответствии с паттерном. Выполнение `match` заканчивается при первом совпадении, т.е. если субъект можно разложить и по `pattern0`, и по `pattern1`, то выполнится только код в `case` блоке с `pattern0`, аналогично блокам `if/elif/elif/…`. Паттерн `case _` выполняет роль `else`, т.е. будет выполнен, если предыдущие паттерны не сработали. Так же как и блок `else`, `case _` является необязательным.

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

Очевидно, что реализация подобного механизма на `if/elif/else` будет менее читабельной. Паттерн матчинг может работать не только со списками, но и со словарями, примитивными значениями, экземплярами классов, он может содержать дополнительные проверки, может быть вложенным. Пример функции получения числа Фибоначчи:

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

## В Python результат паттерн матчинга нельзя присвоить переменной

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

## Паттерн матчинг в Python неисчерпывающий

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

Python не проводит подобные проверки, однако есть инструменты, позволяющие эти проверки добавить, например [Pyright](https://github.com/microsoft/pyright):

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

## Ещё почитать:

[PEP 636](https://peps.python.org/pep-0636/). Structural Pattern Matching: Tutorial

[PEP 635](https://peps.python.org/pep-0635/). Structural Pattern Matching: Motivation and Rationale

[PEP 634](https://peps.python.org/pep-0634/). Structural Pattern Matching: Specification

[Pyright](https://github.com/microsoft/pyright). Static type checker for Python
