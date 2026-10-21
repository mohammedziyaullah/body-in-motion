// www/js/navigation.js
// Client-side instant page navigation and Plotly auto-resize
// Body in Motion — Human Body Response Simulator

function bimShowPage(pageName) {
  if (!pageName) pageName = 'home';
  
  // Hide all page containers
  const pages = document.querySelectorAll('.bim-page-wrap');
  pages.forEach(p => { p.style.display = 'none'; });
  
  // Show target page
  const target = document.getElementById('page_' + pageName + '_wrap');
  if (target) {
    target.style.display = 'block';
  }
  
  // Update navbar active state
  const navBtns = document.querySelectorAll('.bim-nav-btn');
  navBtns.forEach(btn => { btn.classList.remove('active'); });
  const activeBtn = document.getElementById('nav_' + pageName);
  if (activeBtn) {
    activeBtn.classList.add('active');
  }
  
  // Inform Shiny of active page change
  if (window.Shiny && window.Shiny.setInputValue) {
    Shiny.setInputValue('current_active_page', pageName);
  }
  
  // Update browser URL query string without reloading
  if (window.history && window.history.replaceState) {
    const url = new URL(window.location);
    url.searchParams.set('page', pageName);
    window.history.replaceState({}, '', url);
  }
  
  // Trigger window resize so Plotly figures calculate full responsive width
  setTimeout(() => {
    window.dispatchEvent(new Event('resize'));
  }, 100);
}

// =========================================================================
// THEME TOGGLE (DARK MODE <-> LIGHT / DAY MODE)
// =========================================================================
function bimUpdateThemeIcon(isLight) {
  const btn = document.getElementById('theme_toggle');
  if (!btn) return;
  if (isLight) {
    // Sun icon for day / light mode
    btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="5"/><line x1="12" y1="1" x2="12" y2="3"/><line x1="12" y1="21" x2="12" y2="23"/><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/><line x1="1" y1="12" x2="3" y2="12"/><line x1="21" y1="12" x2="23" y2="12"/><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/></svg>';
    btn.setAttribute('title', 'Switch to Dark Theme');
  } else {
    // Moon icon for night / dark mode
    btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"/></svg>';
    btn.setAttribute('title', 'Switch to Day / Light Theme');
  }
}

function bimToggleTheme() {
  const isLight = document.body.classList.toggle('light-mode');
  localStorage.setItem('bim_theme', isLight ? 'light' : 'dark');
  bimUpdateThemeIcon(isLight);
}

// On initial DOM content loaded
document.addEventListener('DOMContentLoaded', function() {
  const params = new URLSearchParams(window.location.search);
  const initial = params.get('page') || 'home';
  bimShowPage(initial);

  // Restore saved theme preference
  const savedTheme = localStorage.getItem('bim_theme');
  if (savedTheme === 'light') {
    document.body.classList.add('light-mode');
    bimUpdateThemeIcon(true);
  } else {
    bimUpdateThemeIcon(false);
  }

  // Attach click handler to theme toggle button
  const themeBtn = document.getElementById('theme_toggle');
  if (themeBtn) {
    themeBtn.addEventListener('click', function(e) {
      e.preventDefault();
      bimToggleTheme();
    });
  }
});
