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
    pricing: number;
  };
}

export class LastWeekdayTrendDescription extends TrendDescription {
  public value0: {
    weekday: number;
    pricing: number;
  };
}

export class SpecificWeekdayTrendDescription extends TrendDescription {
  public value0: {
    weekday: number;
    pricing: number;
  };
}

export class WeekdayTrendDescription extends TrendDescription {
  public value0: {
    // Monday = 1, Sunday = 7
    weekdays: Array<number>;
    pricing: Array<number>;
  };
}

export class EveryWeekdayTrendDescription extends TrendDescription {
  public value0: {
    // mon, tue, wed, thu, fri
    pricing: [number, number, number, number, number];
  };
}

export class WeekendTrendDescription extends TrendDescription {
  public value0: {
    // sat, sun
    pricing: [number, number];
  };
}

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
