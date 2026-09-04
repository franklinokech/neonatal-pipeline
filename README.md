# neonatal-pipeline

Automated ETL from Pumwani & Standard REDCap projects to NEST REDCap (London School).

---

## 🚀 Quick start (Docker)

```bash
git clone https://github.com/franklinokech/neonatal-pipeline.git
cd neonatal-pipeline
cp .env.example .env
# Fill .env with your REDCap tokens and Gmail App Password
docker compose -f docker-compose.yml up --abort-on-container-exit
```

# Development (hot‑reload):
```bash
docker compose -f docker-compose.dev.yml up
```

# Daily cron schedule (6 PM EAT)
```bash
TZ=Africa/Nairobi
0 18 * * * cd /path/to/neonatal-pipeline && docker compose -f docker-compose.yml up --abort-on-container-exit >> logs/cron.log 2>&1
```

# License
MIT