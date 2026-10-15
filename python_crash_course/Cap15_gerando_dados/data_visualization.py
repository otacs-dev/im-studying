import matplotlib.pyplot as plt

input_values = [1, 2, 3, 4, 5]
squares = [1, 4, 9, 16, 25]

# Updated to match modern Matplotlib style naming
plt.style.use('seaborn-v0_8') 
fig, ax = plt.subplots()
ax.scatter(input_values, squares, s=200)  # Added input_values and size for a better plot

# Uncomment these as you progress through Chapter 15:
ax.set_title("Square Numbers", fontsize=24)
ax.set_xlabel("Value", fontsize=14)
ax.set_ylabel("Square of Value", fontsize=14)
ax.tick_params(labelsize=14)

plt.show()


