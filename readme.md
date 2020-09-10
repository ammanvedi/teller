<a href="https://pursuit.purescript.org/packages/purescript-teller">

<img src="https://pursuit.purescript.org/packages/purescript-teller/badge" />

</a>

<a href="https://www.npmjs.com/package/teller-js">

<img src="https://badge.fury.io/js/teller-js.svg" />

</a>

<a href="https://github.com/ammanvedi/teller/actions">

<img src="https://github.com/ammanvedi/teller/workflows/Test%20Library/badge.svg" />

</a>

# Teller

Teller provides trend identification and forecasting facilities based on bank transactions. Teller can be used from both Purescript and Javascript/Typescript.

## Installation

### Purescript

Documentation is published on [Pursuit.](https://pursuit.purescript.org/packages/purescript-teller)

### Javascript & Typescript

`npm install teller-js`

**Teller provides its own Typescript definitions**

## Examples

### Trends

```typescript
// Given some transaction data
import {
  identifyTrends,
  isSpecificWeekdayTrendDescription,
  isEveryWeekdayTrendDescription,
} from "teller-js";

const transactions = [
  {
    accountId: "8Ybo8ppEBrHxJM45p7koSvaKQnM341fyMKQwO",
    timestamp: 1586473200000.0,
    amount: 0.01,
    merchantName: "Asda",
    reference: "Asda",
  },
  ...{
    accountId: "8Ybo8ppEBrHxJM45p7koSvaKQnM341fyMKQwO",
    timestamp: 1586300400000.0,
    amount: -60.0,
    merchantName: "201053 53364402 MOBILE-CHANNEL FT",
    reference: "201053 53364402 MOBILE-CHANNEL FT",
  },
];

const result = identifyTrends(transactions);

result.forEach((tr) => {
  const merchant = tr.value0;
  const trend = tr.value1;

  // See dist/index.d.ts for a full list of all potential trend description types

  if (isEveryWeekdayTrendDescription(trend)) {
    console.log(`${merchant} transaction happens every day`);
  }

  if (isSpecificWeekdayTrendDescription(trend)) {
    // Monday is 1, Sunday is 7
    const days = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"];

    console.log(
      `${merchant} transaction happens every ${days[trend.value0.weekday - 1]}`
    );
  }
});
```

## Further Work

1. Surface algorithm confidence as a percentage in the trend description result
2. Given a start and an end date forecast spending income and balance for each day in the period
3. Given a set of transactions and a trend identify if the trend is holding recently
4. Treat identified trends as fixed costs and provide a estimation of a persons monthly fixed costs
