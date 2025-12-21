import os
import signal
import re
import psutil


def find_processes_like(name_pattern, cmd_pattern=None):
    """
    Finds processes whose name or command line matches a given pattern.
    The pattern can be a simple substring or a regular expression.
    """
    matching_processes = []
    for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
        try:
            process_name = proc.info['name']
            process_cmdline = " ".join(proc.info['cmdline']) if proc.info['cmdline'] else ""

            if not re.search(name_pattern, process_name, re.IGNORECASE):
                continue
            if cmd_pattern:
                if not  re.search(cmd_pattern, process_cmdline, re.IGNORECASE):
                    continue
            matching_processes.append(proc.info)
        except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
            # Handle cases where process might have terminated or access is denied
            pass
    return matching_processes

chrome_query = find_processes_like(r'chrome', r'\\AppData\\Local\\Temp')
chrome_query += find_processes_like(r'chromium', r'\\AppData\\Local\\Temp')
chrome_query += find_processes_like(r'chromedriver.exe')
for process in chrome_query:
    print(f'closing process {process.get("name")} pid:{process.get("pid")}')
    os.kill(process.get("pid"), signal.SIGILL)
