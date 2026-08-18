// First test if this browser supports templates
if ((!"content") in document.createElement("template")) {
  document.getElementById("update-browser-prompt").classList.remove("hidden");
  throw "This browser does not support HTML Template API, please update or change your browser.";
}

// Configuration options
const PUBLICATION_MAP = {
  "3mi2fpvnluk2b": "https://bun.offprint.app",
  "3mqpshjcj422p": "https://bun.leaflet.pub",
  "3mpwlmgh3r4yy": "https://bun.pckt.blog",
};
const COLLECTION_NSIDS = ["site.standard.document"];
const USER_DID = "did:plc:gijpvbkdbr56kazbdjhfvb3d";
const USER_PDS = "https://eurosky.social";
const MAX_LATEST_POSTS = 5;

// Definitions
/** @type {HTMLTemplateElement} */
const ENTRY_TEMPLATE = document.getElementById("leaflet-entry");

const DATE_FORMATTER = new Intl.DateTimeFormat("en-GB", {
  timeStyle: "short",
  dateStyle: "short",
});

/**
 * @param {{ title: string, description: string, url: string, date: Date }} leaflet_entry
 * @returns {HTMLDivElement}
 */
function buildEntry(leaflet_entry) {
  const entry = document.importNode(ENTRY_TEMPLATE.content, true);
  entry.querySelector(".base-anchor").href = leaflet_entry.url;
  entry.querySelector(".title").textContent = leaflet_entry.title;
  const date_el = entry.querySelector(".published-at");
  date_el.datetime = leaflet_entry.date.toISOString();
  date_el.textContent = DATE_FORMATTER.format(leaflet_entry.date);
  entry.querySelector(".description").textContent = leaflet_entry.description;

  return entry;
}

/**
 * @param {string} nsid
 * @returns {Promise<object>}
 */
async function getLatestPosts(nsid) {
  const { records } = await (
    await fetch(
      `${USER_PDS}/xrpc/com.atproto.repo.listRecords?repo=${USER_DID}&collection=${nsid}&limit=${MAX_LATEST_POSTS}`,
    )
  ).json();

  return records.toReversed();
}

// HTML
const leaflet_container = document.getElementById("atproto-leaflet");

document.getElementById("loading").classList.toggle("hidden");
const posts = (
  await Promise.all(
    COLLECTION_NSIDS.map(async (nsid) => await getLatestPosts(nsid)),
  )
)
  .flat(1)
  .sort((entry_a, entry_b) => {
    const date_a = new Date(entry_a.value.publishedAt);
    const date_b = new Date(entry_b.value.publishedAt);

    return date_b - date_a;
  });

for (const document of posts) {
  const { path, site, title, description, publication, publishedAt } =
    document.value;
  const publication_rkey = site.split("/").at(-1);
  const url_start = PUBLICATION_MAP[publication_rkey];

  leaflet_container.append(
    buildEntry({
      title,
      description,
      url: url_start + path,
      date: new Date(publishedAt),
    }),
  );
}

document.getElementById("loading").classList.toggle("hidden");
