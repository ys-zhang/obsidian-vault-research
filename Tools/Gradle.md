#java  #kotlin #DSL 


#  Java without Build Tools

## Compiling

```
# compile java file to class file
javac Prog1.java Prog2.java Prog3.java 

# compile with 3rd party libraries by 
# using `class path`
javac -cp lib1.jar; lib2.jar; lib3.jar Program.java
```

## Packaging into a jar

[Packaging programs in JAR Files](https://docs.oracle.com/javase/tutorial/deployment/jar/)

```
JAR File:

- a manifest file

- class files:
  - xxxx.class
  - yyyy.class

- auxiliary resources
  - ssss.xx
  - tttt.yy
```

> the manifest file specifies meta info such as _comments_, _version_, etc.
> 
> the Jar tool automatically adds a manifest file to the JAR archive with path name `META-INF/MANIFEST.MF`.


| Cmd                                | Operation                            |
| ---------------------------------- | ------------------------------------ |
| `jar cf jar-file input-file(s)`    | create a JAR file                    |
| `jar uf jar-file update-files(s)`  | update a JAR file                    |
| `jar tf jar-file`                  | view a JAR file                      |
| `jar xf jar-file`                  | extract a JAR file                   |
| `jar xf jar-file archived-file(s)` | extract specific files from JAR file |
| `java -jar app.jar`                | run an app packaged as a JAR file    |

- `f` options mean output to a file instead of to the std output
- `c` short for _create_
- `t` short for _table_ of the content
- To indicate which class is the application's _entry point_, you must add a `Main-Class` header to the JAR file's manifest.

## Manifest cheatsheet

[Working with Manifest Files](https://docs.oracle.com/javase/tutorial/deployment/jar/manifestindex.html)

- merge information from an existing file into the manifest file of the JAR file you're creating:
>   jar cfm _jar-file manifest-addition input-file(s)_
- Setting an Application's Entry Point
> Main-Class: _classname_
- Adding Classes to the JAR File's `Classpath`
> Class-Path: _jar1-name jar2-name directory-name/jar3-name_
- Sealing Packages within a JAR File, classes outside the JAR cannot inherit classed defined in the JAR
> Sealed: true
- Version Info

| Header                   | Definition                             |
| ------------------------ | -------------------------------------- |
| `Name`                   | name of the specification              |
| `Specification-Title`    | title of the specification             |
| `Specification-Version`  | version of the specification           |
| `Specification-Vendor`   | The vendor of the specification        |
| `Implementation-Title`   | The title of the implementation        |
| `Implementation-Version` | The build number of the implementation |
| `Implementation-Vendor`  | The vendor of the implementation       | 


# Gradle

| cmd                             | operation                  |
| ------------------------------- | -------------------------- |
| `gradle projects`               | discover project structure |
| `gradle tasks`                  | list all available tasks   |
| `gradle help --task <taskname>` | get task help              |
| `gradle <taskname>`             | run a task                 | 


## Lifecycle

```
# overall lifecycle
init -> config -> execute
```
Lifecycle **Events**:

```kotlin
afterEvaluate { /* some closure */ }
gradle.afterProject { /* some closure, will exc event if project fails */ }
tasks.whenTasksAdded { /* some closure */ }
gradle.taskGraph.beforeTask {}
gradle.taskGraph.afterTask {}
```

| Phase   | Script/Cmd            | Description                                                                                              |
| ------- | --------------------- | -------------------------------------------------------------------------------------------------------- |
| `init`  | `settings.gradle.kts` | which _projects_ will be included in the build, and create and pass the _project object_ to build script |
| config  | `build.gradle.kts`    | executes _build scripts_, create _tasks_.                                                                |
| execute | `gradle/gradlew`      | cli determine which _tasks_ should be executed and run                                                   |

- Settings file `settings.gradle` 
    1. defines the _projects_ taking part in a multi-project build 
    2. add libraries to _build script classpath_
- Build scripts 


## Gradle Configure files

```
project root:
  * build directory
  - build.gradle (build.gradle.kt)
  - gradlew (gradlew.bat)
  - settings.gradle (settings.gradle.kt)
```

- `settings.gradle` is used to setup information about the project such as 
    - `rootProject.name`
- `build.gradle` is the _build script_ config file 
- `gradlew` is the Gradle wrapper script, _always build the project with gradlew_


## Gradle Structure 

![[Pasted image 20221117204427.png]]

- Gradle **tasks** are individual build actions you can run from the command line.
> `./gradlew <task-name>.   # run a gradle task
- When you apply a **plugin** in your _build script_, it automatically _adds tasks_ to your project which you can run to achieve some particular outcome. Common plugins including
    -  [Gradle Java plugin](https://docs.gradle.org/current/userguide/java_plugin.html), automatically adds tasks to compile, test, and package your application, etc.


gradle projects can be _nested_, to include multiple projects in the repository
```kotlin
// settings.gradle.kts

rootProject.name = "petro-tools"  // use petro-tools as proj name instead of directory name
include("lib")                    // subproject included
```

## Multi-project



## Tasks
![[Pasted image 20221118133443.png]]

Almost any build process can be model as a task DAG, where direction represents dependency.

> Applying a plugin will import the tasks defined in the plugin

**Task** consists of 
- **actions**: pieces of work that do something, like copy files or compile sources;
- **inputs**: values, files and directories that _actions_ use or operation on;
- **outputs**: files and directories that the actions modify or generate.

**standard lifecycle tasks**:
1. `clean`: clean generated files
2. `check`: do verifications
3. `assemble`: packaging files and output something ready to publish 
4. `build`: build everything 
5. `build<Configuration>` build artefacts attached to _named configuration_., e.g, `buildArchives`
6. `clean<Task>`: clean files generated from `Task`


## Build Scripts

Each build script is _associated with_ an object of type `Project` and as the build script executes, it configures this `Project`. 

The build script will be compiled into a class implements `Script`(Groovy) or `KotlinBuildScript`(Kotlin) interface, all properties and methods of `Script/KotlinBuildScript` is available in the build script


### Standard Project Properties

| Name          | Type         | Default Value                     |
| ------------- | ------------ | --------------------------------- |
| `project`     | `Project`    | the associated `Project` instance |
| `name`        | `String`     | name of the proj directory        |
| `path`        | `String`     | abs path of the proj              |
| `description` |              |                                   |
| `projectDir`  | `File`       | dir containing the build script   |
| `buildDir`    | `File`       | `projectDir/build`                |
| `group`       | `Object`     | unspecified                       |
| `version`     | `Object`     | unspecified                       |
| `ant`         | `AntBuilder` | an `AntBuilder` instance          |
| `extra`       |              | holding user defined properties   | 


### Example

For how the DSL works see [[#Kotlin Builders and DSL]]

```kotlin
// build.gradle.kts

println(name)         // top level property of the Project obj
println(project.name) // the project property refers to the Proj obj 

// creates and configures a `ScriptHandler` instance
buildscript {
    repositories {
        mavenCentral()
    }
    // this specifies dependencies of the 
    // build script not the project
    // libraries are available in all subprojects
    dependencies {
        "classpath"(
            group = "commons-codec",
            name = "commons-codec",
            version = "1.2"
        )
    }
}

// register a new task under the name "hello"
tasks.register("hello") {
    // declear dependency in task graph
    // hello will not run until someTask finished
    dependsOn("someTask")  
    println("This line will run in the config phase")
    doLast {
        println("Hello world!")
    }
}

tasks.named("hello") { ... }  // manipulating existing tasks

// adding default tasks
// default tasks will run if no tasks are specified in 
// command line arguments
defaultTasks("clean", "run")
```




## The Gradle DSL


```kotlin
plugins {  
    id("java")  // imports the gradle java plugin
}


// where to search and download package dependencies
repositories {  
    mavenCentral()  
}  

// specify dependencies of the project under different builds
dependencies {  
    // only include in the test build
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.9.0")  
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.9.0")  
}


// setup the task jar
jar {
  manifest {
    // filling manifest fields here 
  }
}

```


# Common Gradle Plugins

## Java Plugins

### The `java` plugin

[Doc of java plugin](https://docs.gradle.org/current/userguide/java_plugin.html)

### The `java-library` plugin

[Doc of java-library](https://docs.gradle.org/current/userguide/java_library_plugin.html)

## Other Plugins




# FAQ

- `gradle` and `gradlew`
    - `gradlew` is short for _gradle wrapper_ which is a shell script usually committed to the repository and local to the repository
    - `gradle` is usually an executable installed on the machine and can be globally accessed.
    - the command `gradle init` will create a repository with a `gradlew` script


# References
https://tomgregory.com/gradle-tutorial-for-complete-beginners/

<iframe width="560" height="315" src="https://www.youtube.com/embed/-dtcEMLNmn0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>


[Five things you need to know about Gradle](https://docs.gradle.org/current/userguide/what_is_gradle.html#five_things)
[Using Gradle Plugins](https://docs.gradle.org/current/userguide/plugins.html#sec:subprojects_plugins_dsl()
[The Problem with Gradle](https://www.bruceeckel.com/2021/01/02/the-problem-with-gradle/)


## Kotlin Builders and DSL
[Kotlin type safe builder](https://kotlinlang.org/docs/type-safe-builders.html#how-it-works) 

A HTML DSL

```kotlin
// the DSL example
html {
  head { ... }  // equiv to this.head
  body { ... }  // equiv to this.body
}

class HTML(head: Head, body: Body) : Element() { 
  ... 
}

fun html(init: HTML.() -> Unit): HTML {
  val html = HTML()
  html.init()  // the argument `init` is function with receiver
  return html
}

fun head(init: Head.() -> Unit): Head {...}
fun body(init: Body.() -> Unit): Body {...}

fun <T: Element> initTag(tag: T, init: T.() -> Unit): T { 
  tag.init()
  return tag
}
```






