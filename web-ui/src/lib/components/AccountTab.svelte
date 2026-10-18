<script lang="ts">
  import type { AccountTransactions } from '#lib/model/Application';
  import TxnGroupView from './TxnGroupView.svelte';
  import AmountView from './AmountView.svelte';

  export let accountName: String
  export let accountTxns: AccountTransactions
  export let onFocusAccount: (name: string) => void
</script>

<h4>{accountName}</h4>
{#if !accountTxns || !accountTxns.txnsByMonth || accountTxns.txnsByMonth.length === 0}
  <p>No transactions in this account. Balance: <AmountView amount={accountTxns ? accountTxns.closing : "0.00"} /></p>
{:else}
  <table>
    <tbody>
    {#each accountTxns.txnsByMonth as monthlyTxns}
    <TxnGroupView name={monthlyTxns.name} group={monthlyTxns} txns={monthlyTxns.txns} {onFocusAccount}/>
    {/each}
    </tbody>
  </table>
{/if}

<style>
</style>

