export type TransactionRec = {
  accountId: String;
  timestamp: Number;
  amount: Number;
  merchantName: String;
  reference: String;
};

export class MonthDayTrendDescription {
  value0: {
    dayOfMonth: number;
  };
}

export class LastWeekdayTrendDescription {
  value0: {
    weekday: number;
  };
}

export class SpecificWeekdayTrendDescription {
  value0: {
    weekday: number;
  };
}

export class WeekdayTrendDescription {
  value0: {
    weekdays: Array<number>;
  };
}

export class EveryWeekdayTrendDescription {}

export class WeekendTrendDescription {}

export function isMonthDayTrendDescription(
  inst: any
): inst is MonthDayTrendDescription;

export function isLastWeekdayTrendDescription(
  inst: any
): inst is LastWeekdayTrendDescription;

export function isSpecificWeekdayTrendDescription(
  inst: any
): inst is SpecificWeekdayTrendDescription;

export function isWeekdayTrendDescription(
  inst: any
): inst is WeekdayTrendDescription;

export function isEveryWeekdayTrendDescription(
  inst: any
): inst is EveryWeekdayTrendDescription;

export function isWeekendTrendDescription(
  inst: any
): inst is WeekendTrendDescription;

type TrendDescription =
  | MonthDayTrendDescription
  | LastWeekdayTrendDescription
  | SpecificWeekdayTrendDescription
  | WeekdayTrendDescription
  | EveryWeekdayTrendDescription
  | WeekendTrendDescription;

export type MerchantName = string;

export class Tuple<A, B> {
  value0: A;
  value1: B;
}

export type TrendTuple = Tuple<string, TrendDescription>;

export type Trends = Array<TrendTuple>;

export function identifyTrends(records: Array<TransactionRec>): Trends;
