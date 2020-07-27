export type TransactionRec = {
  accountId: String;
  timestamp: Number;
  amount: Number;
  merchantName: String;
  reference: String;
};

export function getBinaryHeartbeat(
  records: Array<TransactionRec>
): Array<Number>;

export function transactionsOccurrInSuccessiveMonths(
  records: Array<TransactionRec>
): Boolean;
