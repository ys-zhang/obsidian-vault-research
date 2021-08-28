In [[Docker]], a volume is a virtual filesystem that your container can see and use.


# K8S

A [[Kubernetes]] volume is defined at the **pod level**—not the container level.

| Volume type                 | Persistent? (survive if pod die) | Misc.                                           |
| --------------------------- | -------------------------------- | ----------------------------------------------- |
| `EmptyDir`                  | F                                | empty when created                              |
| Network File System (`NFS`) | T                                |                                                 |
| GCEPersistentDisk (`PD`)    | T                                | You can think of a PD as a managed NFS service. |
