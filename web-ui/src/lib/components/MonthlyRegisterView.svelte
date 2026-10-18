<script lang="ts">
  import type { MonthlySummary } from "#lib/model/Application";
  import AmountView from "./AmountView.svelte";

  export let monthlySummaries: MonthlySummary[]
  export let onFocusAccount: (name:string) => void
</script>

<table>
  <tbody>
  {#each monthlySummaries as ms}
    <tr class="month">
      <td>{ms.month}</td>
      <td align="right"><AmountView amount={ms.opening}/></td>
      <td align="right"><AmountView amount={ms.debit} /></td>
      <td align="right"><AmountView amount={ms.credit} /></td>
      <td align="right"><AmountView amount={ms.closing} /></td>
    </tr>
    {#each ms.accountSummaries as as}
      <tr>
        <td class="account" on:click={() => onFocusAccount(as.name)}>{as.name}</td>
        <td align="right"><AmountView amount={as.opening}/></td>
        <td align="right"><AmountView amount={as.debit} /></td>
        <td align="right"><AmountView amount={as.credit} /></td>
        <td align="right"><AmountView amount={as.closing} /></td>
      </tr>
    {/each}
  {/each}
  </tbody>
</table>

<style>
  td {
    padding: 0.33em 0.2em;
    border: 1px solid transparent;
  }
  tr.month {
    font-size: 1.4em;
    font-weight: bold;
  }
  tr.month td {
    background-color: #112;
  }
  td.account:hover {
    background-color: #114;
    cursor: pointer;
  }
  tr:hover td{
    border: 1px solid #88b9
  }
</style>