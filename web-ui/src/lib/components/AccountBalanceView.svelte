<script lang="ts">
  import AccountBalanceView from './AccountBalanceView.svelte';
  import type { AccountBalance } from "#lib/model/Application";
  import { fade } from 'svelte/transition';
  import AmountView from './AmountView.svelte';

  export let prevBreadCrumbs = ""
  export let ab: AccountBalance
  export let level: number = 0
  export let onFocusAccount: (name:string) => void

  $: breadCrumbs = (prevBreadCrumbs.length == 0? "" : prevBreadCrumbs + ":") + ab.name
  $: indent = "&nbsp;|&nbsp;".repeat(level)
  $: nextLevel = level + 1

  $:hasChildren = ab.children.length > 0

  let openChildren = level < 2

  function toggleChildren() {
    openChildren = !openChildren
  }

</script>

<tr class:bold={hasChildren} class:large={level < 1} in:fade|global={{ duration: 200 }} out:fade|global={{duration: 100}}>
  <td>{@html indent}
    {#if hasChildren}
    <button class="name" class:large={level < 1} on:click={toggleChildren}>{ab.name}</button>
    {:else}
    <span class="name" class:large={level < 1}>{ab.name}</span>
    {/if}
    <button on:click={() => onFocusAccount(breadCrumbs)}>☚</button>
  </td>
  <td align="right"><AmountView amount={ab.opening} /></td>
  <td align="right"><AmountView amount={ab.debit} /></td>
  <td align="right"><AmountView amount={ab.credit} /></td>
  <td align="right"><AmountView amount={ab.closing} /></td>
</tr>
{#if openChildren}
  {#each ab.children as child}
    <AccountBalanceView ab={child} level={nextLevel} {onFocusAccount} prevBreadCrumbs={breadCrumbs}/>
  {/each}
{/if}

<style>
  tr:hover {
    background-color: #112;
  }
  .bold {
    font-weight: bold
  }
  .large {
    font-size: 1.2em;
  }
  button.name {
    background-color: #333;
    border-radius: .33em;
    padding: .33em 1em;
    color: #eee;
    border: none;
    font-family: monospace;
  }
  span.name {
    padding: .33em 1em;
    color: #eee;
  }
</style>