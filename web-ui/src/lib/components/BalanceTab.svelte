<script lang="ts">
  import AccountBalanceView from './AccountBalanceView.svelte';
  import type { AccountBalance } from "#lib/model/Application";

  export let accountBalances: AccountBalance[]

  export let onFocusAccount: (name: string) => void
</script>

<h4>Balances</h4>
<table>
  <tbody>
  {#each accountBalances as ab}
    <AccountBalanceView {ab} {onFocusAccount}/>
  {/each}
  </tbody>
</table>

<style>
</style>

