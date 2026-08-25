export interface SelfPost {
  debit: boolean,
  delta: string,
  balance: string
}

export interface Post extends SelfPost {
  name: string
  comments: String[]
}

export type Transaction = {
  date: string,
  indeterminate: boolean,
  comments: String[],
  selfPost: SelfPost,
  posts: Post[]
}

export interface TxnGroup {
  opening: string,
  debit: string,
  credit: string,
  closing: string,
}

export interface AccountBalance extends TxnGroup {
  name: string,
  children: AccountBalance[]
}

interface MonthlyTransactions extends TxnGroup {
  name: string,
  txns: Transaction[],
}

export interface AccountTransactions extends TxnGroup {
  name: string,
  txnsByMonth: MonthlyTransactions[]
}

export interface AccountSummary extends TxnGroup {
  name: string;
}

export interface MonthlySummary extends TxnGroup {
  month: string;
  accountSummaries: AccountSummary[]
}


export type Application = {
  accountBalances: AccountBalance[],
  monthlySummaries: MonthlySummary[],
  accountTxns: Record<string, AccountTransactions>,
  error?: string
}