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

This library is not intended to be a perfect predictor of trends. The aim is for it to be a suggestion engine to provide possible trends to a user who can then correct and modify them as needed.

The sanitised trends can then be fed back into the libraries forecasting function to get accurate predictions of the future state of an account

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

// Finding Trends
const result = identifyTrends(t);

result.forEach((tr) => {
  const merchant = tr.merchant;
  const trend = tr.trend;
	
  // Checking the type of a trend
  if (isEveryWeekdayTrendDescription(trend)) {
    console.log(`${merchant} transaction happens every day`);
  }

  if (isSpecificWeekdayTrendDescription(trend)) {
    const days = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"];

    console.log(
      `${merchant} transaction happens every ${days[trend.value0.weekday]}`
    );
  }
});

// Making a forecast based on the trend, returning which
// trend ids will happen on each day
const f = forecast(1603580400000.0)(1604188800000.0)(result);

console.log(JSON.stringify(f, null, 2));

/**
 * {
  "days": [
    {
      "dateTimestampMs": 1603497600000,
      "trendIds": [
        "Weekday_[2,6,7]_Transport for London"
      ]
    },
    ...
    {
      "dateTimestampMs": 1604102400000,
      "trendIds": [
        "MonthDay_31_BILL_A",
        "MonthDay_31_BILL_B",
        "MonthDay_31_BILL_C",
        "MonthDay_31_BILL_D",
        "Weekday_[2,6,7]_Transport for London"
      ]
    },
    {
      "dateTimestampMs": 1604188800000,
      "trendIds": [
        "MonthDay_1_Ocado",
        "Weekday_[2,6,7]_Transport for London"
      ]
    }
  ]
}
 */
```

## Further Work

1. Surface algorithm confidence as a percentage in the trend description result
2. Add initial balance as an input to forecasting and predict balance at end of forecast and after each day
3. Based on a set of trends provide a user with a total spend on fixed costs at end of forecast
