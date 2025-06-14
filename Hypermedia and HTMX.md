
>[!def] hypermedia
>Hypermedia is a media, for example a text, that includes _non-linear branching_ from one location in the media to another, via, for example, hyperlinks embedded in the media.

>[!example] hypertext
> Hypertext is a new forms of writing, appearing on computer screens, that will branch or perform at the reader’s command. 
>
> A **hypertext** is a _non-sequential_ piece of writing; only the computer display makes it practical.

>[!def] Hypermedia Control
> A **hypermedia control** is an element in a hypermedia that _describes (or controls) some sort of interaction, often with a remote server_, by encoding information about that interaction directly and completely within itself.

>[!def] Hypermedia-Driven Application (HDA)
> A web application that uses _hypermedia_ and _hypermedia exchanges_ as its primary mechanism for communicating with a server.
i

# Event Sequence for an HTMX GET Request

1. **Triggering Event**  
    The initial DOM event (e.g., `click`, `input`, or custom event) that activates the HTMX request.
    
2. **`htmx:configRequest`**  
    Fires before the request is sent. Use this to modify request parameters (e.g., headers, URL, payload).  
    _Cancelable_: Yes (call `event.preventDefault()` to abort).
```javascript
document.addEventListener(
  "htmx:configRequest", 
  (event) => {
    // Add/modify parameters
    event.detail.parameters.customParam = "value"; 
  }
);
``` 

3. **`htmx:beforeRequest`**  
    Fires just before the AJAX call is made. Useful for showing loading indicators.  
    _Cancelable_: Yes.
4. **`htmx:beforeSend`**  
    Fires after `beforeRequest` but before the request is sent. Can modify the `XMLHttpRequest` object.  
    _Cancelable_: Yes.
5. **`htmx:send`**  
    Fires as the request is sent to the server. No cancellation here; request is in flight.
6. **Server Processes Request**  
    The server handles the GET request and returns a response.
7. **Response Handling**
    - **Success**:
        - **`htmx:beforeOnLoad`**: Fires before processing the response. Modify the response here.  
            **Cancelable**: Yes.
        - **`htmx:afterOnLoad`**: Fires after processing the response (before DOM updates).
    - **Error** (e.g., network failure, 4xx/5xx status):
        - **`htmx:responseError`**: Fires on HTTP errors. Useful for error logging or fallback UI.
8. **DOM Updates**
    - **`htmx:beforeSwap`**: Fires before swapping content into the target element. Modify the DOM or response here.  
        **Cancelable**: Yes.
    - **`htmx:afterSwap`**: Fires after new content is swapped into the DOM.
9. **Settling Phase**
    - **`htmx:beforeSettle`**: Fires before CSS transitions/animations are processed.
    - **`htmx:afterSettle`**: Fires after all transitions/animations complete.