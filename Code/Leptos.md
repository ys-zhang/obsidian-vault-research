- [Leptos Document](https://docs.rs/leptos/latest/leptos/)
- [Leptos Repo](https://github.com/gbj/leptos)


# reactive system

terminology:

| Leptos | Rx         |
| ------ | ---------- |
| signal | observable |
| effect | observer   | 

```rust 
pub(crate) struct Runtime {
    pub shared_context: RefCell<Option<SharedContext>>,
    pub observer: Cell<Option<EffectId>>,
    pub scopes: RefCell<SlotMap<ScopeId, RefCell<Vec<ScopeProperty>>>>,
    pub scope_parents: RefCell<SparseSecondaryMap<ScopeId, ScopeId>>,
    pub scope_children: RefCell<SparseSecondaryMap<ScopeId, Vec<ScopeId>>>,
    #[allow(clippy::type_complexity)]
    pub scope_contexts: RefCell<SparseSecondaryMap<ScopeId, HashMap<TypeId, Box<dyn Any>>>>,
    #[allow(clippy::type_complexity)]
    pub scope_cleanups: RefCell<SparseSecondaryMap<ScopeId, Vec<Box<dyn FnOnce()>>>>,
    pub signals: RefCell<SlotMap<SignalId, Rc<RefCell<dyn Any>>>>,  // why Rc<RefCell<>>?
    pub signal_subscribers: RefCell<SecondaryMap<SignalId, RefCell<HashSet<EffectId>>>>,
    pub effects: RefCell<SlotMap<EffectId, Rc<dyn AnyEffect>>>,
    pub effect_sources: RefCell<SecondaryMap<EffectId, RefCell<HashSet<SignalId>>>>,
    pub resources: RefCell<SlotMap<ResourceId, AnyResource>>,
}

// runtime.scopes: RefCell<SlotMap<ScopeId, RefCell<Vec<ScopeProperty>>>>
pub(crate) enum ScopeProperty {
    Signal(SignalId),
    Effect(EffectId),
    Resource(ResourceId),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ReadSignal<T>
where
    T: 'static,  
{
    pub(crate) runtime: RuntimeId,
    pub(crate) id: SignalId,
    pub(crate) ty: PhantomData<T>,
}

```

for static trait bound see [](https://doc.rust-lang.org/rust-by-example/scope/lifetime/static_lifetime.html#trait-bound)
