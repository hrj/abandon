<script lang="ts">
  import AccountTab from './AccountTab.svelte';
  import BalanceTab from './BalanceTab.svelte';
  import type { Application } from "$lib/model/Application";
  import { TabType, BalanceTabInfo, type TabInfo, AccountTabInfo, MonthlyRegisterTabInfo } from '$lib/model/Tabs';
  import MonthlyRegisterView from './MonthlyRegisterView.svelte';
  import { selectedAmounts } from '$lib/stores';

  export let application: Application

  let statsError = false;
  let sum = 0;
  let avg = 0;
  let min = 0;
  let max = 0;
  let count = 0;

  $: {
    if ($selectedAmounts.length > 0) {
      statsError = false;
      let values = [];
      for (const sa of $selectedAmounts) {
        // Remove commas before parsing
        const val = parseFloat(sa.amount.replace(/,/g, ''));
        if (isNaN(val)) {
          statsError = true;
          break;
        }
        values.push(val);
      }

      if (!statsError && values.length > 0) {
        count = values.length;
        sum = values.reduce((a, b) => a + b, 0);
        avg = sum / count;
        min = Math.min(...values);
        max = Math.max(...values);
      }
    }
  }

  function clearSelection() {
    $selectedAmounts = [];
  }

  let tabs: TabInfo[] = [
    new BalanceTabInfo(),
    new MonthlyRegisterTabInfo()
  ]

  let currentTabIndex = 0
  $: currentTab = tabs[currentTabIndex]

  function onFocusAccount(name: string) {
    currentTabIndex = tabs.push(new AccountTabInfo(name)) - 1
    tabs = tabs
  }

  function closeTab(index: number) {
    if (tabs[index].closeable) {
      tabs.splice(index, 1)
      currentTabIndex = Math.min(currentTabIndex, tabs.length - 1)
      tabs = tabs
    }
  }
</script>

<div>
  {#if application.error}
    <div class="error-banner">
      <span class="error-icon">⚠️</span>
      <span class="error-message">{application.error}</span>
    </div>
  {/if}

  <div class="header">
    <ul>
      {#each tabs as tab, index}
      <li class:selected={index == currentTabIndex}>
        <div>
        <span on:click={() => currentTabIndex = index}>{tab.getTitle()}</span>
        {#if tab.closeable}
          <span class="closeButton" on:click={() => closeTab(index)}>X</span>
        {/if}
        </div>
      </li>
      {/each}
    </ul>

    {#if $selectedAmounts.length > 0}
      <div class="stats">
        {#if statsError}
          <span class="error">Error parsing amounts</span>
        {:else}
          <span>[{count}]</span>
          <span>Sum: {sum.toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}</span>
          <span>Avg: {avg.toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}</span>
          <span>Min: {min.toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}</span>
          <span>Max: {max.toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}</span>
        {/if}
        <button on:click={clearSelection} title="Deselect all">X</button>
      </div>
    {/if}
  </div>

  <div class="contents">
    {#if currentTab.tabType == TabType.Balance}
      <BalanceTab accountBalances={application.accountBalances} {onFocusAccount} />
    {/if}
    {#if currentTab instanceof AccountTabInfo}
      <AccountTab accountName={currentTab.name} accountTxns={application.accountTxns[currentTab.name]} {onFocusAccount}/>
    {/if}
    {#if currentTab instanceof MonthlyRegisterTabInfo}
      <MonthlyRegisterView monthlySummaries={application.monthlySummaries} {onFocusAccount} />
    {/if}
  </div>

</div>

<style>
  .header {
    position: sticky;
    top: 0px;
    background-color: #0007;
    backdrop-filter: blur(.3em);
    z-index: 10;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  .stats {
    display: flex;
    gap: 1em;
    padding: 0.5em 1em;
    background-color: #222;
    border: 1px solid #444;
    border-radius: 4px;
    margin-right: 1em;
    color: #eee;
    font-family: monospace;
    align-items: center;
  }
  .stats .error {
    color: #ff5555;
  }
  .stats button {
    background: #444;
    border: none;
    color: white;
    cursor: pointer;
    border-radius: 3px;
    padding: 2px 6px;
    font-size: 0.9em;
  }
  .stats button:hover {
    background: #666;
  }
  ul {
    display: flex;
    flex-wrap: wrap;
    padding: 0;
    margin: 0;
  }
  li {
    text-decoration: none;
    list-style: none;
    padding: .33em 1em;
    background-color: #111;
    margin: .5em;
    border-radius: .1em;
    color: #aaa;
    text-transform: uppercase;
    font-family: monospace;
    font-size: 1.2em;
    border: 1px solid #333;
  }
  li:hover {
    cursor: pointer
  }
  li.selected {
    background-color: #335;
    border: 1px solid #555;
    color: #eee;
    font-weight: bold;
  }
  .closeButton {
    padding: .33em 1em;
    border:1px solid #111;
    background-color: #222;
    font-size: .8em;
    opacity: 0.7;
    font-family:sans-serif;
  }
  .error-banner {
    background-color: #d32f2f;
    color: #ffffff;
    padding: 0.75em 1.25em;
    display: flex;
    align-items: center;
    gap: 0.75em;
    font-family: sans-serif;
    font-size: 1em;
    font-weight: bold;
    box-shadow: 0 2px 5px rgba(0,0,0,0.3);
  }
  .error-icon {
    font-size: 1.2em;
  }
  .contents {
    padding: 1em;
  }
</style>