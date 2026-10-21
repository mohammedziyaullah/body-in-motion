import urllib.request, re, os

for act in ['AEROBIC', 'ANAEROBIC', 'STRESS']:
    url = f'https://physionet.org/files/wearable-device-dataset/1.0.1/Wearable_Dataset/{act}/'
    req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
    try:
        with urllib.request.urlopen(req, timeout=15) as r:
            html = r.read().decode('utf-8')
            subs = re.findall(r'href=[\'"]([^\'"/]+)/[\'"]', html)
            subs = [s for s in subs if s != '..']
            print(f'{act}: {len(subs)} participants -> {subs[:10]}')
    except Exception as e:
        print(f'{act} error: {e}')
