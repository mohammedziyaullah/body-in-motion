import os
import urllib.request
import concurrent.futures
import time

RAW_DIR = r"C:\Users\moham\.gemini\antigravity\scratch\body-in-motion\data\raw\Wearable_Dataset"
BASE_URL = "https://physionet.org/files/wearable-device-dataset/1.0.1/Wearable_Dataset"

ACTIVITIES = ["AEROBIC", "ANAEROBIC", "STRESS"]
# High quality subjects covering Stage 1 (S) and Stage 2 (f), males and females, varied body stats:
PARTICIPANTS = ["S01", "S02", "S04", "S05", "S08", "S09", "S10", "f01", "f02", "f03", "f04", "f05"]
# Core physiological signals:
SIGNALS = ["HR.csv", "EDA.csv", "TEMP.csv", "tags.csv", "ACC.csv"]

def download_file(activity, subject, signal):
    rel_path = os.path.join(activity, subject, signal)
    out_path = os.path.join(RAW_DIR, rel_path)
    if os.path.exists(out_path) and os.path.getsize(out_path) > 100:
        return (rel_path, "already_exists", os.path.getsize(out_path))
    
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    url = f"{BASE_URL}/{activity}/{subject}/{signal}?download"
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            data = resp.read()
            with open(out_path, "wb") as f:
                f.write(data)
            return (rel_path, "success", len(data))
    except urllib.error.HTTPError as e:
        return (rel_path, f"http_{e.code}", 0)
    except Exception as e:
        return (rel_path, f"error_{str(e)}", 0)

def main():
    print("Downloading essential signals for participants across all activities...", flush=True)
    tasks = []
    # Small files first (HR, EDA, TEMP, tags)
    for sig in ["HR.csv", "EDA.csv", "TEMP.csv", "tags.csv", "ACC.csv"]:
        for act in ACTIVITIES:
            for sub in PARTICIPANTS:
                tasks.append((act, sub, sig))
                
    print(f"Total tasks: {len(tasks)}", flush=True)
    t0 = time.time()
    success_count = 0
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as executor:
        future_to_task = {executor.submit(download_file, act, sub, sig): (act, sub, sig) for act, sub, sig in tasks}
        for future in concurrent.futures.as_completed(future_to_task):
            res = future.result()
            if "success" in res[1] or "already_exists" in res[1]:
                success_count += 1
                if success_count % 5 == 0 or "success" in res[1]:
                    print(f"[{success_count}] {res[0]} ({res[1]}, {res[2]} B)", flush=True)
            elif "http_404" in res[1]:
                pass
            else:
                print(f"Note {res[0]}: {res[1]}", flush=True)
                
    elapsed = time.time() - t0
    print(f"Completed! Downloaded/verified {success_count} files in {elapsed:.1f}s.", flush=True)

if __name__ == "__main__":
    main()
