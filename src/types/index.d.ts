export type TransactionRec = {
  accountId: String;
  timestamp: Number;
  amount: Number;
  merchantName: String;
  reference: String;
};

export class TrendDescription {}

export class MonthDayTrendDescription extends TrendDescription {
  public value0: {
    dayOfMonth: number;
  };
}

export class LastWeekdayTrendDescription extends TrendDescription {
  public value0: {
    weekday: number;
  };
}

export class SpecificWeekdayTrendDescription extends TrendDescription {
  public value0: {
    weekday: number;
  };
}

export class WeekdayTrendDescription extends TrendDescription {
  public value0: {
    weekdays: Array<number>;
  };
}

export class EveryWeekdayTrendDescription extends TrendDescription {}

export class WeekendTrendDescription extends TrendDescription {}

export function isMonthDayTrendDescription(
  inst: TrendDescription
): inst is MonthDayTrendDescription;

export function isLastWeekdayTrendDescription(
  inst: TrendDescription
): inst is LastWeekdayTrendDescription;

export function isSpecificWeekdayTrendDescription(
  inst: TrendDescription
): inst is SpecificWeekdayTrendDescription;

export function isWeekdayTrendDescription(
  inst: TrendDescription
): inst is WeekdayTrendDescription;

export function isEveryWeekdayTrendDescription(
  inst: TrendDescription
): inst is EveryWeekdayTrendDescription;

export function isWeekendTrendDescription(
  inst: TrendDescription
): inst is WeekendTrendDescription;

export type MerchantName = string;

export class Tuple<A, B> {
  value0: A;
  value1: B;
}

export type TrendTuple = Tuple<string, TrendDescription>;

export type Trends = Array<TrendTuple>;

export function identifyTrends(records: Array<TransactionRec>): Trends;
