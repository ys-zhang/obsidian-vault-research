- [What is server side rendering](https://blog.somewhatabstract.com/2020/01/13/what-is-server-side-rendering-ssr/)
- [Hydration and SSR](https://blog.somewhatabstract.com/2020/03/16/hydration-and-server-side-rendering/)

# Why SSR

In SPA, the page is received from the server in an initial state and then subsequent data requests may populate that page (imagine your Facebook feed loading) to get it ready for you to use.

However, this can mean that _the time to interactive – the length of time before a user can actually use the page – is long_. 

This affects all sorts of things, but particularly user retention. Folks don't like waiting and if they wait too long, they become frustrated and eventually bounce.

To get a nice experience for our users, _code runs on the backend to build an initial page and then that is handed to the frontend_, which promptly takes over.

> The real problem here is how to assign workload/responsibilities to frontend and backend, i.e., which part should be run on server side?

The Server Side Rendering handles the problem by first "render" the virtual DOM into real Html DOM.

# Hydration

With SSR, client will receive a DOM which is well stuffed with content; however, `js` scripts are not attach to the DOM, it is the client's responsibility to attach `js` scripts, i.e., to hydrate.

Rules of hydration:
1. the initial render cycle must result in the same markup, whether run on the server or the client;
2. call `vdom.hydrate` instead of `vdom.render` on the client.






