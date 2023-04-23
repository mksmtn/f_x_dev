# ngx-remotedata: корректная типизация данных от сервера

Когда мы пишем фронт, нам постоянно приходится отображать данные, полученные с сервера. По крайней мере, мы надеемся, что мы их получим. Но иногда наше wishful thinking не оправдывается, и данные долго не подгружаются или не подгружаются вообще. Часто разработчики забывают обработать состояния загрузки и ошибки, что приводит к недоумению пользователей и даже к лишним обращениям в поддержку. Например, раньше ВК при долгой загрузке списка друзей отображал «У вас нет друзей». Я сам недавно обратился в поддержку МТС из-за того, что у них в приложении не отобразился баланс. Мне ответили: «Проверьте соединение, выключите VPN», и, действительно, после подключения к WiFi баланс отобразился.

Код, приводящий к подобному субоптимальному пользовательскому опыту, вероятно, выглядит примерно так:

```ts
@Component({
  selector: 'app-balance',
  template: `
  <div *ngIf="balance || balance === 0">
    {{ balance | number }} руб.
  </div>
  `,
})
export class BalanceComponent {
  @Input() balance?: number;
}
```

Проблема очевидна — не учтены случаи ошибки и долгой загрузки. Обычно с ними борются, добавляя дополнительные поля, такие как `loading` и `error`:

```ts
@Component({
  selector: 'app-balance',
  template: `
  <div *ngIf="loading">
    Баланс загружается...
  </div>
  <div *ngIf="error">
    {{ error }}
  </div>
  <div *ngIf="balance || balance === 0">
    {{ balance | number }} руб.
  </div>
  `,
})
export class BalanceComponent {
  @Input() balance?: number;
  @Input() loading = false;
  @Input() error?: string;
}
```

Можно объединить `balance`, `loading`, `error` в один объект, можно получать данные от сервиса, а не через `Input`, суть не в этом. Фундаментальные недостатки останутся:

- Такая типизация позволяет объявить логически невозможные состояния, например `{balance: 5, loading: true, error: 'No connection'}`;
- Легко забыть проверить состояния загрузки и ошибки, а ведь проверять нужно не только в шаблоне компонента, но и в TS коде, в сервисах.

Проблему можно решить, используя *discriminated unions*:

```ts
type BalanceRemoteData =
  | { tag: 'not asked'; }  
  | { tag: 'loading'; }
  | { tag: 'success'; balance: number; }
  | { tag: 'failure'; error: string; }

// Или более абстрактно
type RemoteData<D, E = string> =
  | { tag: 'not asked'; }
  | { tag: 'loading'; }
  | { tag: 'success'; data: D; }
  | { tag: 'failure'; error: E; }

type BalanceRemoteData = RemoteData<number>;
```

Теперь перед использованием `data` в сервисе или компоненте необходимо сначала убедиться, что данные успешно загружены, иначе TS будет ругаться:

```ts
// работает
function getBalanceInUSD(balanceRemoteData: RemoteData<number>): RemoteData<number> {
  if (balanceRemoteData.tag === 'success') {
    const usdRubRate = 1 / 25;  // верните мой 2007
    return {
      tag: 'success',
      data: balanceRemoteData.data * usdRubRate,
    };
  } else {
    return balanceRemoteData;
  }
}

// ошибка компиляции
function getBalanceInUSD(balanceRemoteData: RemoteData<number>): RemoteData<number> {
  const usdRubRate = 1 / 25;
  return {
    tag: 'success',
    // Property 'data' does not exist on type 'RemoteData<number, string>'.
    // Property 'data' does not exist on type '{ tag: "not asked"; }'.ts(2339)
    data: balanceRemoteData.data * usdRubRate,
  };
}
```

Чтобы не создавать в каждом проекте тип `RemoteData`, и чтобы получить множество полезных функций и ангуляровских `pipe`'ов для работы с этим типом, есть готовая библиотека [ngx-remotedata](https://github.com/joanllenas/ngx-remotedata). Благодаря ей функцию `getBalanceInUSD` можно переписать следующим образом:

```ts
import { RemoteData, map, success, inProgress, failure } from 'ngx-remotedata';

const usdRubRate = 1 / 25;

const getBalanceInUSD = (balanceRemoteData: RemoteData<number>) =>
  map((balance) => balance * usdRubRate, balanceRemoteData); 

getBalanceInUSD(success(50));  // now success(2) (in USD)
getBalanceInUSD(inProgress());  // still in progress
getBalanceInUSD(failure('ops'));  // still failure
```

А компонент может выглядеть следующим образом:

```ts
import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RemoteData, RemoteDataModule, notAsked } from 'ngx-remotedata';

@Component({
  standalone: true,
  selector: 'app-balance',
  template: `
  <div *ngIf="balance | isInProgress">
    Баланс загружается...
  </div>
  <div *ngIf="balance | isSuccess">
    <div *ngIf="balance | successValue as blnc; else emptyBalanceTemplate">
      {{ blnc | number }} руб.
    </div>
    <ng-template #emptyBalanceTemplate>
      Время пополнить счёт, баланс 0 руб.
    </ng-template>
  </div>
  <div *ngIf="balance | isFailure">
    {{ balance | failureError }}
  </div>
  `,
  imports: [CommonModule, RemoteDataModule],
})
export class BalanceComponent {
  @Input()
  balance: RemoteData<number> = notAsked();
}
```

Полезно думать о типе `RemoteData` как об обертке над вычислением (*computational context*), хранящей дополнительные данные о состоянии, в котором находится выражение.

Таким образом, `ngx-remotedata` избавляет нас от ошибки, когда во время ожидания ответа от сервера мы показываем состояние для нулевых данных («У вас нет друзей»), а также напоминает нам о необходимости обработать состояние загрузки и ошибки выполнения запроса, причём не только в шаблонах компонентов, но и в TS коде.

## Ещё почитать

- [Документация](https://github.com/joanllenas/ngx-remotedata) `ngx-remotedata`

- [How Elm Slays a UI Antipattern](http://blog.jenkster.com/2016/06/how-elm-slays-a-ui-antipattern.html)
