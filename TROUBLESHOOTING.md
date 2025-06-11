# Troubleshooting Guide

This guide helps you diagnose and resolve common issues with the MCP Selenium Haskell server.

## Quick Diagnostics

Before diving into specific issues, run these quick checks:

1. **Verify Selenium server is running**:
   ```bash
   curl http://localhost:4444/status
   ```

2. **Check environment variables**:
   ```bash
   echo "SELENIUM_HOST: ${SELENIUM_HOST:-127.0.0.1}"
   echo "SELENIUM_PORT: ${SELENIUM_PORT:-4444}"
   ```

3. **Test server connectivity**:
   ```bash
   mcp-selenium-hs --help
   ```

## Connection Issues

### Problem: WebDriver connection failed (E001)

**Symptoms:**
- Server fails to start browser sessions
- Error messages about connection refused
- Timeouts when attempting to connect

**Solutions:**

1. **Check if Selenium server is running**:
   ```bash
   # For standalone selenium server
   curl http://localhost:4444/status

   # For Selenium Grid
   curl http://selenium-hub:4444/grid/api/hub/status
   ```

2. **Verify network connectivity**:
   ```bash
   # Test port connectivity
   nc -zv localhost 4444

   # For remote servers
   nc -zv selenium.example.com 4444
   ```

3. **Check environment variables**:
   ```bash
   export SELENIUM_HOST=your-selenium-host
   export SELENIUM_PORT=4444
   ```

4. **Start Selenium server if missing**:
   ```bash
   # Download selenium-server-standalone
   wget https://github.com/SeleniumHQ/selenium/releases/download/selenium-4.15.0/selenium-server-4.15.0.jar

   # Start server
   java -jar selenium-server-4.15.0.jar standalone
   ```

### Problem: Session creation timeout

**Symptoms:**
- Browser sessions take too long to start
- Timeout errors during start_browser calls
- High resource usage during session creation

**Solutions:**

1. **Check system resources**:
   ```bash
   # Check available memory
   free -h

   # Check CPU usage
   top
   ```

2. **Verify browser binaries are installed**:
   ```bash
   # Chrome
   google-chrome --version
   chromium --version

   # Firefox
   firefox --version
   ```

3. **Try headless mode for better performance**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "headless": true
     }
   }
   ```

4. **Check for conflicting browser processes**:
   ```bash
   # Kill existing browser processes
   pkill chrome
   pkill firefox
   ```

## Element Interaction Issues

### Problem: Element not found (E032)

**Symptoms:**
- find_element returns "Element not found"
- Selectors that work in DevTools fail in automation
- Intermittent element location failures

**Solutions:**

1. **Verify element exists on the page**:
   - Open the page manually in a browser
   - Use DevTools to test your selector
   - Check if the element is in an iframe

2. **Wait for page to fully load**:
   ```json
   {
     "session_id": "your-session-id",
     "by": "css",
     "value": "#your-element",
     "timeout": 15000
   }
   ```

3. **Test selectors in browser DevTools**:
   ```javascript
   // In browser console
   document.querySelector("#your-element")
   document.querySelectorAll(".your-class")
   $x("//your/xpath")
   ```

4. **Check if element is in iframe**:
   ```javascript
   // Check for iframes
   document.querySelectorAll('iframe')
   ```

5. **Use more robust selectors**:
   ```json
   // Instead of position-based XPath
   "//div[1]/span[2]/button"

   // Use content or attributes
   "//button[contains(text(), 'Submit')]"
   "//button[@data-testid='submit-btn']"
   ```

### Problem: Element not clickable (E043)

**Symptoms:**
- click_element fails even though element exists
- "Element is not clickable" errors
- Clicks don't register on the page

**Solutions:**

1. **Ensure element is visible**:
   ```javascript
   // Check element visibility in DevTools
   const element = document.querySelector('#your-element');
   const style = window.getComputedStyle(element);
   console.log('Display:', style.display);
   console.log('Visibility:', style.visibility);
   console.log('Opacity:', style.opacity);
   ```

2. **Wait for element to become clickable**:
   ```json
   {
     "session_id": "your-session-id",
     "by": "css",
     "value": "#your-element",
     "timeout": 10000
   }
   ```

3. **Check if element is covered by other elements**:
   ```javascript
   // Check what element is at the click coordinates
   const element = document.querySelector('#your-element');
   const rect = element.getBoundingClientRect();
   const centerX = rect.left + rect.width / 2;
   const centerY = rect.top + rect.height / 2;
   const topElement = document.elementFromPoint(centerX, centerY);
   console.log('Element at click point:', topElement);
   ```

4. **Scroll element into view first**:
   ```javascript
   // Use find_element first, then click
   element.scrollIntoView({behavior: 'smooth', block: 'center'});
   ```

## Performance Issues

### Problem: Operations are slow

**Symptoms:**
- Tools take much longer than expected timeouts
- Browser feels sluggish
- High CPU or memory usage

**Solutions:**

1. **Use headless mode**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "headless": true,
       "arguments": [
         "--no-sandbox",
         "--disable-dev-shm-usage",
         "--disable-gpu"
       ]
     }
   }
   ```

2. **Optimize browser arguments**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "headless": true,
       "arguments": [
         "--no-sandbox",
         "--disable-dev-shm-usage",
         "--disable-gpu",
         "--disable-background-timer-throttling",
         "--disable-renderer-backgrounding",
         "--disable-features=VizDisplayCompositor",
         "--disable-extensions",
         "--disable-plugins",
         "--disable-images"
       ]
     }
   }
   ```

3. **Reduce implicit wait timeouts**:
   ```json
   {
     "timeout": 5000
   }
   ```

4. **Use more efficient selectors**:
   ```
   Preferred order (fastest to slowest):
   1. ID: "#element-id"
   2. CSS: ".class-name"
   3. CSS: "tag[attribute='value']"
   4. XPath: "//tag[@attribute='value']"
   5. XPath with text: "//tag[contains(text(), 'text')]"
   ```

### Problem: High memory usage

**Symptoms:**
- System memory usage increases over time
- Browser processes consume excessive RAM
- System becomes unresponsive

**Solutions:**

1. **Close sessions when no longer needed**:
   ```json
   {
     "tool": "close_browser",
     "arguments": {
       "session_id": "your-session-id"
     }
   }
   ```

2. **Limit number of concurrent sessions**:
   ```bash
   # Monitor active sessions
   ps aux | grep chrome
   ps aux | grep firefox
   ```

3. **Clear browser cache periodically**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "arguments": [
         "--disable-application-cache",
         "--disable-background-timer-throttling",
         "--disable-renderer-backgrounding"
       ]
     }
   }
   ```

## Logging Issues

### Problem: Console logs not captured

**Symptoms:**
- get_console_logs returns empty results
- JavaScript errors not appearing in logs
- Console.log statements not captured

**Solutions:**

1. **Enable logging in browser options**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "headless": true
     },
     "enableLogging": true
   }
   ```

2. **Check available log types**:
   ```json
   {
     "tool": "get_available_log_types",
     "arguments": {
       "session_id": "your-session-id"
     }
   }
   ```

3. **Use injected console logger for comprehensive monitoring**:
   ```json
   {
     "tool": "inject_console_logger",
     "arguments": {
       "session_id": "your-session-id",
       "timeout": 30000
     }
   }
   ```

4. **Verify browser supports requested log levels**:
   ```json
   {
     "tool": "get_console_logs",
     "arguments": {
       "session_id": "your-session-id",
       "logLevel": "ALL"
     }
   }
   ```

### Problem: Log injection fails

**Symptoms:**
- inject_console_logger returns errors
- JavaScript injection timeouts
- Injected logs are empty

**Solutions:**

1. **Ensure page allows JavaScript execution**:
   - Check for Content Security Policy restrictions
   - Verify page has finished loading

2. **Inject logger before page navigation**:
   ```json
   // Navigate first
   {
     "tool": "navigate",
     "arguments": {
       "session_id": "your-session-id",
       "url": "https://example.com"
     }
   }

   // Then inject logger
   {
     "tool": "inject_console_logger",
     "arguments": {
       "session_id": "your-session-id"
     }
   }
   ```

3. **Increase injection timeout**:
   ```json
   {
     "tool": "inject_console_logger",
     "arguments": {
       "session_id": "your-session-id",
       "timeout": 60000
     }
   }
   ```

## Browser-Specific Issues

### Chrome Issues

**Problem: Chrome crashes or fails to start**

Solutions:
```json
{
  "browser": "chrome",
  "options": {
    "arguments": [
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--disable-gpu",
      "--disable-software-rasterizer",
      "--disable-background-timer-throttling"
    ]
  }
}
```

**Problem: Chrome in Docker containers**

Solutions:
```json
{
  "browser": "chrome",
  "options": {
    "headless": true,
    "arguments": [
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--disable-gpu",
      "--remote-debugging-port=9222",
      "--disable-features=VizDisplayCompositor"
    ]
  }
}
```

### Firefox Issues

**Problem: Firefox compatibility issues**

Solutions:
```json
{
  "browser": "firefox",
  "options": {
    "headless": true,
    "arguments": [
      "--safe-mode",
      "--width=1920",
      "--height=1080"
    ]
  }
}
```

**Problem: GeckoDriver version mismatch**

Solutions:
1. Check Firefox and GeckoDriver compatibility:
   ```bash
   firefox --version
   geckodriver --version
   ```

2. Download compatible GeckoDriver from:
   https://github.com/mozilla/geckodriver/releases

## Environment-Specific Issues

### Docker/Container Issues

**Problem: Browser fails to start in container**

Solutions:
1. **Add required capabilities**:
   ```dockerfile
   FROM ubuntu:20.04
   RUN apt-get update && apt-get install -y \
       chromium-browser \
       firefox \
       xvfb
   ```

2. **Use appropriate Chrome flags**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "headless": true,
       "arguments": [
         "--no-sandbox",
         "--disable-dev-shm-usage",
         "--disable-gpu",
         "--disable-software-rasterizer"
       ]
     }
   }
   ```

### CI/CD Environment Issues

**Problem: Tests fail in CI but work locally**

Solutions:
1. **Use headless mode in CI**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "headless": true
     }
   }
   ```

2. **Increase timeouts for slower CI environments**:
   ```json
   {
     "timeout": 30000
   }
   ```

3. **Add CI-specific browser arguments**:
   ```json
   {
     "browser": "chrome",
     "options": {
       "arguments": [
         "--no-sandbox",
         "--disable-dev-shm-usage",
         "--disable-gpu",
         "--window-size=1920,1080"
       ]
     }
   }
   ```

## Debugging Techniques

### Enable Debug Logging

Set enableLogging to true for verbose output:
```json
{
  "browser": "chrome",
  "options": {
    "headless": false
  },
  "enableLogging": true
}
```

### Use Browser DevTools

1. **Start browser in non-headless mode** for visual debugging
2. **Open DevTools** to inspect elements and test selectors
3. **Monitor Network tab** for AJAX requests and timing
4. **Check Console tab** for JavaScript errors

### Test Incrementally

1. **Test individual operations** rather than complex workflows
2. **Use simple selectors first** (ID, CSS) before complex XPath
3. **Add explicit waits** between operations
4. **Verify each step** before proceeding to the next

### Monitor System Resources

```bash
# Monitor memory usage
watch -n 1 'free -h'

# Monitor browser processes
watch -n 1 'ps aux | grep -E "(chrome|firefox)" | grep -v grep'

# Monitor network connections
netstat -an | grep :4444
```

## Getting Help

If you're still experiencing issues:

1. **Check the integration tests** in `integration_tests/` for working examples
2. **Review the API documentation** in `API.md`
3. **Enable debug logging** to get more detailed error information
4. **Create a minimal reproduction case** with the simplest possible scenario
5. **Check the project issues** on GitHub for similar problems

### Useful Log Information

When reporting issues, include:
- Browser type and version
- Operating system
- Selenium server version
- Complete error messages
- MCP tool parameters used
- Environment variables set

### Minimal Reproduction Example

```json
{
  "tool": "start_browser",
  "arguments": {
    "browser": "chrome",
    "options": {
      "headless": true
    },
    "enableLogging": true
  }
}
```

This guide covers the most common issues. For additional help, refer to the project documentation or create an issue with detailed information about your specific problem.
