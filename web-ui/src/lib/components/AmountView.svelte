<script lang="ts">
  import { onDestroy } from 'svelte';
  import { selectedAmounts } from '#lib/stores';

  export let amount: string

  const id = crypto.randomUUID();
  let isSelected = false;

  function handleClick(event: MouseEvent) {
    if (event.ctrlKey || event.metaKey) {
      isSelected = !isSelected;
      if (isSelected) {
        $selectedAmounts = [...$selectedAmounts, { id, amount }];
      } else {
        $selectedAmounts = $selectedAmounts.filter(a => a.id !== id);
      }
    }
  }

  $: {
    if ($selectedAmounts.length === 0 && isSelected) {
      isSelected = false;
    } else if (isSelected && !$selectedAmounts.some(a => a.id === id)) {
      isSelected = false;
    }
  }

  onDestroy(() => {
    if (isSelected) {
      selectedAmounts.update(amounts => amounts.filter(a => a.id !== id));
    }
  });
</script>

<!-- svelte-ignore a11y_click_events_have_key_events -->
<!-- svelte-ignore a11y_no_static_element_interactions -->
<span
  class:subdued={amount == "0.00" && !isSelected}
  class:selected={isSelected}
  on:click={handleClick}
>{amount}</span>

<style>
  .subdued {
    opacity: 0.2;
  }
  .selected {
    background-color: #5555aa; /* subtle blue */
    color: #ffffff;
    border-radius: 3px;
    padding: 1px 3px;
    cursor: pointer;
  }
</style>
