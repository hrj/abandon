<script lang="ts">
  import { dev } from "$app/env";
  import MainPage from "#lib/components/MainPage.svelte";
  import type { Application } from "#lib/model/Application";
  import { onMount, onDestroy } from "svelte";
  import { selectedAmounts } from "#lib/stores";
  import { get } from "svelte/store";

  let application: Application | null = null

  const URL = dev ? "/sample.json" : "/api/"

  let timeoutId: number | null = null;

  async function fetchAndUpdate() {
    if (get(selectedAmounts).length === 0) {
      const result = await fetch(URL)
      const json = await result.json()
      application = json;
    }
    // Note: use window.setTimeout to avoid type issues with NodeJS.Timeout in browser context
    timeoutId = window.setTimeout(fetchAndUpdate, 4_000);
  }

  onMount(async () => {
    fetchAndUpdate()
  })

  onDestroy(() => {
    if (timeoutId) {
      clearTimeout(timeoutId);
    }
  })

</script>

{#if application}
  <MainPage {application}/>
{:else}
  <p>Loading...</p>
{/if}

