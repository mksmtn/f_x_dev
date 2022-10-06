# Маппинг типов в Typescript на примере

В этой статье я расскажу о маппинге типов и об условных типах в TypeScript на примере. Недавно мне пришлось работать с сервисом, который возвращает ответ в виде JSON, у которого все примитивные значения представлены строками.

```json
{
  "name": "Joe",
  "age": "19",
  "married": "true",
  "friends": [
    {
      "name": "Alice",
      "age": "26",
      "married": "false"
    }
  ],
  "job": {
    "title": "gardener",
    "salary": "500",
    "onVacation": "false"
  }
}
```

Очевидно, что с таким объектом работать неудобно. `age` и `salary` должны быть числами, а `married` и `onVacation` — булиевыми значениями. Поэтому нужно смаппить ответ от сервиса так, чтобы значения полей обрели логический и числовой тип там, где эти типы подразумевались.

```ts
interface Person {
  name: string;
  age: number;
  married: boolean;
}

interface Profile extends Person {
  friends: Array<Person>;
  job: {
    title: string;
    salary: number;
    onVacation: boolean;
  };
}

function parseBadJSON(badJSON: any): Profile {
  // mapping logic
}
```

Но как корректно описать тип параметра `badJSON`, чтобы TS не позволил нам ошибиться в маппинге? Сейчас ничего не мешает внутри функции сделать опечатку вроде `married: badJSON.maried` (должно быть две `r`) или другую ошибку. Конечно, можно скопировать тип `Profile` и написать рядом ещё один `ProfileInput`, в котором вручную заменить все `number` и `boolean` на `string`, но по понятным причинам это решение далеко от идеального, особенно если тип `Profile` в реальной жизни будет более сложным. К счастью, в TS есть несколько фич, позволяющих рекурсивно заменить все `number` и `boolean` на `string` на любом уровне вложенности: маппинг типов ([mapped types](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html)) и условные типы ([conditional types](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html)). Вот как может выглядеть реализация:

```ts
type AllStrings<T> = {
  [Key in keyof T]: T[Key] extends number
    ? string
    : T[Key] extends boolean
    ? string
    : AllStrings<T[Key]>;
};

function parseBadJSON(badJSON: AllStrings<Profile>): Profile {
  // mapping logic
}
```

Тип `AllStrings` работает следующим образом. Для переданного типа `T` он заменяет тип каждого его поля в зависимости от условий. Если это поле имеет числовой тип `T[Key] extends number`, то оно заменяется на `string`, если поле имееет логический тип `T[Key] extends boolean`, то тоже заменяет на строковый. В противном случае рекурсивно применяется `AllStrings<T[Key]>`. Что интересно, здесь нет необходимости прописывать кейс `T[Key] extends string ? string : AllStrings<T[Key]>`. Когда TS встречает примитивный тип внутри маппинга типов, как в случае `AllStrings<string>`, он возвращает сам примитивный тип, т.е. в данном случае `string`. Это поведение описано в [TypeScript-FAQ](https://github.com/microsoft/TypeScript/wiki/FAQ#common-bugs-that-arent-bugs)

## References

[1] TypeScript Handbook: [Mapped types](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html)

[2] TypeScript Handbook: [Conditional types](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html)

[3] TypeScript [FAQ](https://github.com/microsoft/TypeScript/wiki/FAQ#common-bugs-that-arent-bugs)
