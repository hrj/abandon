<script lang="ts">
  import type { Transaction, TxnGroup } from "#lib/model/Application";
  import AmountView from "./AmountView.svelte";

  export let name: string | null
  export let group: TxnGroup|null
  export let txns: Transaction[]
  export let onFocusAccount: (name:string) => void
</script>

{#if group && name}
  <tr class="groupHead">
    <td class="name">{name}</td>
    <td></td>
    <td align="right"><AmountView amount={group.opening} /></td>
    <td align="right"><AmountView amount={group.debit} /></td>
    <td align="right"><AmountView amount={group.credit} /></td>
    <td align="right"><AmountView amount={group.closing} /></td>
  </tr>
{/if}
{#each txns as t, index}
  <tr class:odd={index % 2 != 0}>
    <td>{t.date}</td>
    <td class:indeterminate={t.indeterminate}>
      {#if t.posts.length > 1}
        <p>Compound txn</p>
        <table class="details">
        {#each t.posts as p}
          <tr class:even={index % 2 == 0}>
            <td>
              <span>{p.name}</span>
              <button on:click={() => onFocusAccount(p.name)}>☚</button>
              <div class="comments">
                {#each p.comments as comment}
                  <p class="comment">{comment}</p>
                {/each}
              </div>
            </td>
            {#if p.debit}
            <td align="right">{p.delta}</td> 
            <td></td> 
            {:else}
            <td></td> 
            <td align="right">{p.delta}</td> 
            {/if}
          </tr>
        {/each}
        </table>
      {:else}
        <span>{t.posts[0].name}</span>
        <button on:click={() => onFocusAccount(t.posts[0].name)}>☚</button>
        <div class="comments">
          {#each t.posts[0].comments as comment}
            <p class="comment">{comment}</p>
          {/each}
        </div>
      {/if}

      <div class="comments">
        {#each t.comments as comment}
          <p class="comment">{comment}</p>
        {/each}
      </div>
    </td>
    <td></td>
    {#if t.selfPost.debit}
    <td align="right"><AmountView amount={t.selfPost.delta}/></td> 
    <td></td> 
    {:else}
    <td></td> 
    <td align="right"><AmountView amount={t.selfPost.delta}/></td> 
    {/if}
    <td align="right"><AmountView amount={t.selfPost.balance}/></td>
  </tr>
{/each}

<style>
  tr {
    vertical-align: top;
    border: 1px solid transparent
  }
  tr:hover td {
    border: 1px solid #88b9
  }
  td {
    padding: .33em 1rem;
  }
  tr.groupHead {
    background-color: #112;
    font-size: 1.1em;
  }
  tr.groupHead td.name {
    padding: 0.33em;
    font-size: 1.3em;
  }
  tr.odd {
    background-color: #252525;
  }
  table.details {
    background-color: #292929;
    margin-right: 1em;
  }
  .indeterminate {
    background-color: #311;
  }
  .comments {
    border-left: .5em solid #333;
    padding-left: 1em;
    margin-left: 1em;
    opacity: 0.7;
  }
</style>