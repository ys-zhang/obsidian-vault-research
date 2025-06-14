#ui-test 

# **Testing Library Tutorial: A Beginner's Guide**  

Testing Library is a family of libraries that helps you test UI components **like a real user would**, focusing on behavior rather than implementation details. It works with React, Vue, Angular, Svelte, and even plain DOM.  

This tutorial covers:  
✅ **Core Principles** of Testing Library  
✅ **Queries** (how to find elements)  
✅ **User Interactions** (`fireEvent` vs `user-event`)  
✅ **Best Practices** for maintainable tests  

---

## **1. Setup**  
First, install Testing Library in your project.  

### **For React**  
```bash
npm install @testing-library/react @testing-library/jest-dom @testing-library/user-event --save-dev
```
- `@testing-library/react` → React testing utilities  
- `@testing-library/jest-dom` → Extra DOM assertions (`toBeVisible`, `toHaveTextContent`)  
- `@testing-library/user-event` → Simulate real user interactions  

### **For Vue**  
```bash
npm install @testing-library/vue @testing-library/jest-dom @testing-library/user-event --save-dev
```

### **For Plain JavaScript (DOM)**  
```bash
npm install @testing-library/dom @testing-library/user-event --save-dev
```

---

## **2. Core Principles**  
Testing Library encourages:  
🔹 **Testing behavior, not implementation** (avoid testing `props`/`state`).  
🔹 **Querying elements like a user would** (by text, role, label).  
🔹 **Avoiding `container.querySelector`** (use `getByRole`, `getByText` instead).  

---

## **3. Basic Example: Testing a Button**  

### **Component (`Button.js`)**
```jsx
const Button = ({ onClick, children }) => (
  <button onClick={onClick}>{children}</button>
);
export default Button;
```

### **Test File (`Button.test.js`)**
```jsx
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import Button from './Button';

test('button renders and responds to clicks', async () => {
  const handleClick = jest.fn(); // Mock function
  render(<Button onClick={handleClick}>Click Me</Button>);

  // Find button by text
  const button = screen.getByText('Click Me');

  // Check if button is in the document
  expect(button).toBeInTheDocument();

  // Simulate user click
  await userEvent.click(button);

  // Verify onClick was called
  expect(handleClick).toHaveBeenCalledTimes(1);
});
```

### **Key Takeaways:**
✔ `render()` → Renders the component.  
✔ `screen` → Global DOM query methods (`getByText`, `getByRole`).  
✔ `userEvent.click()` → Simulates a real click (better than `fireEvent`).  

---

## **4. Querying Elements**  
Prefer **accessible queries** in this order:  

| Query                   | Example                               | Best For                        |
| ----------------------- | ------------------------------------- | ------------------------------- |
| `getByRole`             | `getByRole('button')`                 | Buttons, links, form elements   |
| `getByLabelText`        | `getByLabelText('Username')`          | Inputs with labels              |
| `getByPlaceholderText`  | `getByPlaceholderText('Enter email')` | Inputs with placeholders        |
| `getByText`             | `getByText('Submit')`                 | Text content                    |
| `getByTestId`[^test-id] | `getByTestId('submit-btn')`           | Last resort (not user-friendly) |

[^test-id]: `getByTestId` is equivalent to ``{js} container.querySelector(`[data-testid="${yourId}"]`)``


### **Example: Form Testing**
```jsx
test('form submission works', async () => {
  render(<LoginForm />);

  // Find inputs by their labels
  const emailInput = screen.getByLabelText('Email');
  const passwordInput = screen.getByLabelText('Password');
  const submitButton = screen.getByRole('button', { name: 'Submit' });

  // Simulate typing
  await userEvent.type(emailInput, 'test@example.com');
  await userEvent.type(passwordInput, 'password123');

  // Submit form
  await userEvent.click(submitButton);

  // Assert something happened (e.g., success message)
  expect(await screen.findByText('Login successful!')).toBeInTheDocument();
});
```

---

## **5. `fireEvent` vs `user-event`**  
| Feature | `fireEvent` | `user-event` |
|---------|------------|-------------|
| **Realism** | Basic events | Simulates full interactions |
| **Typing** | `fireEvent.change(input, { target: { value: 'text' } })` | `userEvent.type(input, 'text')` |
| **Clicks** | `fireEvent.click(button)` | `userEvent.click(button)` (handles hover, focus) |
| **Recommended?** | Use if `user-event` isn’t available | **✅ Preferred** (more realistic) |

---

## **6. Best Practices**  
✔ **Use `findBy` for async elements** (waits for appearance).  
✔ **Avoid `container.querySelector`** (breaks user-centric testing).  
✔ **Mock APIs** (`MSW` or `jest.mock`).  
✔ **Keep tests simple** (1 assertion per test ideally).  

---

## **7. Advanced Example: Testing a Fetch Component**  
```jsx
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import FetchData from './FetchData';

test('loads and displays data', async () => {
  // Mock fetch
  global.fetch = jest.fn(() =>
    Promise.resolve({
      json: () => Promise.resolve({ name: 'John Doe' }),
    })
  );

  render(<FetchData />);

  // Click the "Load Data" button
  await userEvent.click(screen.getByText('Load Data'));

  // Wait for data to appear
  expect(await screen.findByText('John Doe')).toBeInTheDocument();

  // Cleanup mock
  fetch.mockClear();
});
```

---

## **8. Conclusion**  
✅ **Testing Library** helps write **maintainable, user-focused tests**.  
✅ **Prefer `getByRole` and `user-event`** for realistic interactions.  
✅ **Avoid testing implementation details** (like state/props).  

---

### **Next Steps**  
- [Official Docs](https://testing-library.com/)  
- Try integrating with **Vitest** or **Jest**.  
- Learn **Mock Service Worker (MSW)** for API mocking.  

Would you like a deeper dive into **React Testing Library** or **Vue Testing Library**? 🚀