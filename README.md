# Alpine huts map beta

GitHub Pages should publish the `docs/` folder.

Minimal static beta:
- `docs/index.html`
- `docs/huts.geojson`
- `docs/availability.json`
- `docs/config.js`

The included workflow only rebuilds `availability.json` from an existing `data/calendar_results.xlsx`. The full scraper still needs a non-interactive login before it can run unattended on GitHub Actions.
