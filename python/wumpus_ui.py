import tkinter as tk
from tkinter import ttk
import os
from PIL import Image, ImageTk


# Force colors to work the same in dark mode and light mode
BG_LIGHT_BLUE = "#E0F2F7"  # Main background
BUTTON_BG = "#EBF8FF"  # Button fill color
BUTTON_BORDER = "#D0E0F0"  # Button border (light gray)
TEXT_DARK = "#606060"  # Dark gray text
WHITE = "#FFFFFF"  # White for borders and cells
BLACK = "#000000"  # Black for grid borders and text


class App(tk.Tk):

	def __init__(self) -> None:
		super().__init__()
		self.title("Wumpus World")
		# Force background color regardless of system theme
		self.configure(bg=BG_LIGHT_BLUE)
		self.geometry("880x560")
		self.minsize(720, 480)

		# Force ttk to use our colors (especially on macOS)
		try:
			style = ttk.Style(self)
			style.theme_use("clam")
			# Override default colors to force our theme
			style.configure("TFrame", background=BG_LIGHT_BLUE)
			style.configure("TLabel", background=BG_LIGHT_BLUE, foreground=TEXT_DARK)
			style.configure("TButton", background=BUTTON_BG, foreground=TEXT_DARK)
		except Exception:
			pass

		# Settings state shared across screens
		self.settings = {
			"difficulty": 1,
			"pits": 1,
			"wumpus": 1,
		}

		self.container = tk.Frame(self, bg=BG_LIGHT_BLUE)
		self.container.pack(fill=tk.BOTH, expand=True)

		self.frames = {}
		for F in (MainMenuFrame, SettingsFrame, GameFrame):
			frame = F(parent=self.container, controller=self)
			self.frames[F.__name__] = frame
			frame.place(relx=0, rely=0, relwidth=1, relheight=1)

		self.show_frame("MainMenuFrame")

	def show_frame(self, name: str) -> None:
		self.frames[name].tkraise()
		if hasattr(self.frames[name], "on_show"):
			self.frames[name].on_show()


class RoundedButton(tk.Canvas):
	"""Rounded rectangular button matching the image design - horizontally elongated with light blue fill and border"""

	def __init__(self, parent, text="", command=None, width=200, height=50):
		super().__init__(parent, width=width, height=height, bg=BG_LIGHT_BLUE, highlightthickness=0)
		self._btn_w = width
		self._btn_h = height
		self._command = command
		self._text = text
		self._draw()
		self.bind("<Button-1>", self._on_click)
		self.bind("<Enter>", self._on_enter)
		self.bind("<Leave>", self._on_leave)

	def _draw(self):
		self.delete("all")  # Clear canvas
		# Main rounded rectangle button with light blue fill and slightly darker border
		r = self._btn_h // 2  # Radius for rounded corners (pill shape)
		# Draw filled rounded rectangle
		self._draw_round_rect(0, 0, self._btn_w, self._btn_h, r, fill=BUTTON_BG, outline=BUTTON_BORDER, width=1)
		# Text centered in button
		self.create_text(
			self._btn_w // 2, 
			self._btn_h // 2, 
			text=self._text, 
			fill=TEXT_DARK, 
			font=("Helvetica", 14, "normal")
		)

	def _draw_round_rect(self, x1, y1, x2, y2, r, fill, outline, width=1):
		# Draw rounded rectangle using rectangles and arcs for pill-shaped button
		# Center rectangles
		self.create_rectangle(x1 + r, y1, x2 - r, y2, fill=fill, outline="", width=0)
		self.create_rectangle(x1, y1 + r, x2, y2 - r, fill=fill, outline="", width=0)
		# Corner arcs (rounded ends)
		self.create_oval(x1, y1, x1 + 2 * r, y1 + 2 * r, fill=fill, outline="", width=0)
		self.create_oval(x2 - 2 * r, y1, x2, y1 + 2 * r, fill=fill, outline="", width=0)
		self.create_oval(x1, y2 - 2 * r, x1 + 2 * r, y2, fill=fill, outline="", width=0)
		self.create_oval(x2 - 2 * r, y2 - 2 * r, x2, y2, fill=fill, outline="", width=0)
		# Border outline
		if width > 0:
			# Draw border as separate arcs and lines
			# Top and bottom horizontal lines
			self.create_line(x1 + r, y1, x2 - r, y1, fill=outline, width=width)
			self.create_line(x1 + r, y2, x2 - r, y2, fill=outline, width=width)
			# Left and right vertical lines
			self.create_line(x1, y1 + r, x1, y2 - r, fill=outline, width=width)
			self.create_line(x2, y1 + r, x2, y2 - r, fill=outline, width=width)
			# Corner arcs for border
			self.create_arc(x1, y1, x1 + 2 * r, y1 + 2 * r, start=90, extent=90, style=tk.ARC, outline=outline, width=width)
			self.create_arc(x2 - 2 * r, y1, x2, y1 + 2 * r, start=0, extent=90, style=tk.ARC, outline=outline, width=width)
			self.create_arc(x1, y2 - 2 * r, x1 + 2 * r, y2, start=180, extent=90, style=tk.ARC, outline=outline, width=width)
			self.create_arc(x2 - 2 * r, y2 - 2 * r, x2, y2, start=270, extent=90, style=tk.ARC, outline=outline, width=width)

	def _on_click(self, _):
		if callable(self._command):
			self._command()

	def _on_enter(self, _):
		# Slight highlight on hover - can add subtle effect here if needed
		pass

	def _on_leave(self, _):
		pass


class MainMenuFrame(tk.Frame):
	"""Main menu screen with Start and Setting buttons"""

	def __init__(self, parent, controller):
		super().__init__(parent, bg=BG_LIGHT_BLUE)
		self.controller = controller

		# Container for buttons
		btn_container = tk.Frame(self, bg=BG_LIGHT_BLUE)
		btn_container.place(relx=0.5, rely=0.5, anchor="center")

		# Start button
		start_btn = RoundedButton(
			btn_container, 
			text="Start", 
			command=lambda: controller.show_frame("GameFrame"),
			width=220,
			height=55
		)
		start_btn.pack(pady=20)

		# Setting button
		settings_btn = RoundedButton(
			btn_container,
			text="Setting",
			command=lambda: controller.show_frame("SettingsFrame"),
			width=220,
			height=55
		)
		settings_btn.pack(pady=20)

		# Bottom area with light blue background
		bottom_pattern = tk.Frame(self, bg=BG_LIGHT_BLUE, height=60)
		bottom_pattern.pack(side=tk.BOTTOM, fill=tk.X)

	def on_show(self):
		self.focus_set()


class GameGrid(tk.Frame):
	"""3x6 game grid"""

	def __init__(self, parent):
		super().__init__(parent, bg=BG_LIGHT_BLUE)
		self.rows = 3
		self.cols = 6
		self.cells = []
		self._build()

	def _build(self):
		# Create grid frame
		grid_frame = tk.Frame(self, bg=BG_LIGHT_BLUE)
		grid_frame.pack(pady=20)

		for r in range(self.rows):
			row = []
			for c in range(self.cols):
				# White cell with black border
				cell = tk.Frame(
					grid_frame, 
					width=80, 
					height=80, 
					bg=WHITE, 
					highlightbackground=BLACK, 
					highlightthickness=1
				)
				cell.grid(row=r, column=c, padx=0, pady=0, sticky="nsew")
				
				# Configure grid weights
				grid_frame.grid_columnconfigure(c, weight=1, uniform="col")
				grid_frame.grid_rowconfigure(r, weight=1, uniform="row")
				
				# Label for content (icons, text, etc.)
				cell_label = tk.Label(cell, text="", bg=WHITE, fg=BLACK, font=("Helvetica", 12))
				cell_label.place(relx=0.5, rely=0.5, anchor="center")
				row.append(cell_label)
			self.cells.append(row)

	def paint_demo_layout(self):
		"""Paint the exact layout from the image"""
		# Clear all cells first
		for row in self.cells:
			for cell in row:
				cell.configure(text="", bg=WHITE, fg=BLACK)
		
		# Row 1 (Top, index 0)
		self.cells[0][0].configure(text="🔥", bg=WHITE, fg="#FF0000")  # Red flame
		self.cells[0][1].configure(text="", bg=WHITE)  # Empty
		self.cells[0][2].configure(text="", bg=WHITE)  # Empty
		self.cells[0][3].configure(text="💨", bg=WHITE, fg=BLACK)  # Wind (black)
		self.cells[0][4].configure(text="●", bg=WHITE, fg=BLACK, font=("Helvetica", 20))  # Pit (black blob)
		self.cells[0][5].configure(text="💨", bg=WHITE, fg=BLACK)  # Wind (black)
		
		# Row 2 (Middle, index 1)
		self.cells[1][0].configure(text="🏆\n🚚", bg=WHITE)  # Trophy (gold) and cart (red)
		self.cells[1][1].configure(text="🔥", bg=WHITE, fg="#FF0000")  # Red flame
		self.cells[1][2].configure(text="", bg=WHITE)  # Empty
		# Character with flag and text
		char_text = "🚩\n👤\njb2.jar"
		self.cells[1][3].configure(text=char_text, bg=WHITE, fg=BLACK, font=("Helvetica", 9))
		self.cells[1][4].configure(text="💨", bg=WHITE, fg=BLACK)  # Wind (black)
		self.cells[1][5].configure(text="", bg=WHITE)  # Empty
		
		# Row 3 (Bottom, index 2)
		self.cells[2][0].configure(text="🔥", bg=WHITE, fg="#FF0000")  # Red flame
		self.cells[2][1].configure(text="", bg=WHITE)  # Empty
		self.cells[2][2].configure(text="", bg=WHITE)  # Empty
		self.cells[2][3].configure(text="", bg=WHITE)  # Empty
		self.cells[2][4].configure(text="", bg=WHITE)  # Empty
		self.cells[2][5].configure(text="💨", bg=WHITE, fg=BLACK)  # Wind (black)


class GameFrame(tk.Frame):
	"""Game screen with 3x6 grid, navigation, and shoot button"""

	def __init__(self, parent, controller):
		super().__init__(parent, bg=BG_LIGHT_BLUE)
		self.controller = controller

		# Game grid (3 rows x 6 columns)
		self.grid_panel = GameGrid(self)
		self.grid_panel.place(relx=0.5, rely=0.35, anchor="center")

		# Settings gear icon in top-right - using Flaticon icon
		# Using Label instead of Button to completely eliminate any border
		# Get the project root directory (parent of python directory)
		script_dir = os.path.dirname(os.path.abspath(__file__))
		project_root = os.path.dirname(script_dir)
		icon_path = os.path.join(project_root, "images", "settings_icon.png")
		
		gear_icon = None
		gear_icon_photo = None
		
		if os.path.exists(icon_path):
			try:
				# Load and resize icon - made bigger (32x32 instead of 24x24)
				gear_image = Image.open(icon_path)
				# Resize to appropriate size for UI (slightly bigger)
				gear_image = gear_image.resize((32, 32), Image.Resampling.LANCZOS)
				# Convert to PhotoImage for tkinter
				gear_icon_photo = ImageTk.PhotoImage(gear_image)
				gear_icon = gear_icon_photo
			except Exception as e:
				print(f"Could not load settings icon: {e}")
				gear_icon = None
		else:
			print(f"Settings icon not found at {icon_path}. Using text fallback.")
		
		# Use Label instead of Button to avoid any border appearance
		gear_btn = tk.Label(
			self,
			image=gear_icon if gear_icon else None,
			text="⚙" if not gear_icon else "",
			bg=BG_LIGHT_BLUE,
			fg=BLACK,
			font=("Helvetica", 24) if not gear_icon else ("Helvetica", 20),
			cursor="hand2"
		)
		# Bind click event to the label
		gear_btn.bind("<Button-1>", lambda e: controller.show_frame("SettingsFrame"))
		# Store reference to prevent garbage collection
		if gear_icon_photo:
			gear_btn.image = gear_icon_photo
		gear_btn.place(relx=0.95, rely=0.05, anchor="ne")

		# Navigation buttons (horizontal row below grid)
		nav_frame = tk.Frame(self, bg=BG_LIGHT_BLUE)
		nav_frame.place(relx=0.5, rely=0.65, anchor="center")

		# Left arrow button
		left_btn = RoundedButton(
			nav_frame,
			text="←",
			width=60,
			height=45
		)
		left_btn.grid(row=0, column=0, padx=12)

		# Up arrow button
		up_btn = RoundedButton(
			nav_frame,
			text="↑",
			width=60,
			height=45
		)
		up_btn.grid(row=0, column=1, padx=12)

		# Right arrow button
		right_btn = RoundedButton(
			nav_frame,
			text="→",
			width=60,
			height=45
		)
		right_btn.grid(row=0, column=2, padx=12)

		# Shoot button (below nav buttons, slightly to the left)
		shoot_btn = RoundedButton(
			self,
			text="Shoot",
			width=120,
			height=45,
			command=self._on_shoot
		)
		shoot_btn.place(relx=0.25, rely=0.78, anchor="center")

	def _on_shoot(self):
		# Placeholder for shoot action
		pass

	def on_show(self):
		self.grid_panel.paint_demo_layout()
		self.focus_set()


class SettingsRow(tk.Frame):
	"""A row in settings with label, -/value/+ controls"""

	def __init__(self, parent, label, get_value, set_value, show_controls=True):
		super().__init__(parent, bg=BUTTON_BG)
		self.get_value = get_value
		self.set_value = set_value
		self.show_controls = show_controls

		# Label
		lab = tk.Label(
			self, 
			text=label, 
			bg=BUTTON_BG, 
			fg=TEXT_DARK, 
			font=("Helvetica", 14)
		)
		lab.pack(side=tk.LEFT, padx=(0, 8))

		if show_controls:
			# Minus button - square button with light blue bg and thin light gray border
			minus_frame = tk.Frame(
				self,
				bg=BUTTON_BORDER,
				width=32,
				height=32
			)
			minus_frame.pack(side=tk.LEFT, padx=6)
			minus_frame.pack_propagate(False)
			minus_btn = tk.Button(
				minus_frame,
				text="-",
				bg=BUTTON_BG,
				fg=TEXT_DARK,
				font=("Helvetica", 14),
				border=0,
				relief=tk.FLAT,
				activebackground=BUTTON_BG,
				activeforeground=TEXT_DARK,
				command=self.dec,
				cursor="hand2",
				highlightthickness=1,
				highlightbackground=BUTTON_BORDER,
				highlightcolor=BUTTON_BORDER
			)
			minus_btn.pack(fill=tk.BOTH, expand=True, padx=1, pady=1)

		# Value field - square text field with light blue bg and thin light gray border (if controls shown)
		# Otherwise just plain text
		if show_controls:
			value_frame = tk.Frame(
				self,
				bg=BUTTON_BORDER,
				width=32,
				height=32
			)
			value_frame.pack(side=tk.LEFT, padx=6)
			value_frame.pack_propagate(False)
			self.value_lbl = tk.Label(
				value_frame,
				text=str(self.get_value()),
				bg=BUTTON_BG,
				fg=TEXT_DARK,
				font=("Helvetica", 14),
				highlightthickness=1,
				highlightbackground=BUTTON_BORDER
			)
			self.value_lbl.pack(fill=tk.BOTH, expand=True, padx=1, pady=1)
		else:
			# Just plain text label without border
			self.value_lbl = tk.Label(
				self,
				text=str(self.get_value()),
				bg=BUTTON_BG,
				fg=TEXT_DARK,
				font=("Helvetica", 14)
			)
			self.value_lbl.pack(side=tk.LEFT, padx=6)

		if show_controls:
			# Plus button - square button with light blue bg and thin light gray border
			plus_frame = tk.Frame(
				self,
				bg=BUTTON_BORDER,
				width=32,
				height=32
			)
			plus_frame.pack(side=tk.LEFT, padx=6)
			plus_frame.pack_propagate(False)
			plus_btn = tk.Button(
				plus_frame,
				text="+",
				bg=BUTTON_BG,
				fg=TEXT_DARK,
				font=("Helvetica", 14),
				border=0,
				relief=tk.FLAT,
				activebackground=BUTTON_BG,
				activeforeground=TEXT_DARK,
				command=self.inc,
				cursor="hand2",
				highlightthickness=1,
				highlightbackground=BUTTON_BORDER,
				highlightcolor=BUTTON_BORDER
			)
			plus_btn.pack(fill=tk.BOTH, expand=True, padx=1, pady=1)

	def refresh(self):
		if hasattr(self, 'value_lbl'):
			self.value_lbl.configure(text=str(self.get_value()))

	def inc(self):
		self.set_value(self.get_value() + 1)
		self.refresh()

	def dec(self):
		v = self.get_value()
		if v > 0:
			self.set_value(v - 1)
			self.refresh()


class SettingsFrame(tk.Frame):
	"""Settings screen with configuration panel"""

	def __init__(self, parent, controller):
		super().__init__(parent, bg=BG_LIGHT_BLUE)
		self.controller = controller

		# Main settings panel (rounded rectangle with white border)
		# Use a frame with border to simulate the white border
		panel_container = tk.Frame(self, bg=BG_LIGHT_BLUE)
		panel_container.place(relx=0.5, rely=0.45, anchor="center", relwidth=0.54, relheight=0.42)
		
		# White border frame (simulating rounded rectangle with white border)
		panel_border = tk.Frame(
			panel_container,
			bg=WHITE,
			highlightbackground=WHITE,
			highlightthickness=1
		)
		panel_border.pack(fill=tk.BOTH, expand=True, padx=0, pady=0)
		
		# Inner panel
		inner_panel = tk.Frame(panel_border, bg=BUTTON_BG)
		inner_panel.pack(fill=tk.BOTH, expand=True, padx=2, pady=2)

		# Settings rows container
		rows_frame = tk.Frame(inner_panel, bg=BUTTON_BG)
		rows_frame.pack(padx=30, pady=30, anchor="w")

		# Difficulty level row
		self.row_difficulty = SettingsRow(
			rows_frame,
			"Difficulty level:",
			lambda: controller.settings["difficulty"],
			lambda v: controller.settings.__setitem__("difficulty", max(1, v)),
			show_controls=True
		)
		self.row_difficulty.pack(anchor="w", pady=15)

		# No. of pits row
		self.row_pits = SettingsRow(
			rows_frame,
			"No. of pits:",
			lambda: controller.settings["pits"],
			lambda v: controller.settings.__setitem__("pits", max(0, v)),
			show_controls=True
		)
		self.row_pits.pack(anchor="w", pady=15)

		# No. of wumpus row (no controls, just display)
		self.row_wumpus = SettingsRow(
			rows_frame,
			"No. of wumpus:",
			lambda: controller.settings["wumpus"],
			lambda v: controller.settings.__setitem__("wumpus", v),
			show_controls=False
		)
		self.row_wumpus.pack(anchor="w", pady=15)

		# Save & Go Back button
		save_btn = RoundedButton(
			self,
			text="Save & Go Back",
			command=lambda: controller.show_frame("MainMenuFrame"),
			width=200,
			height=50
		)
		save_btn.place(relx=0.5, rely=0.82, anchor="center")

		# Bottom area with light blue background
		bottom_pattern = tk.Frame(self, bg=BG_LIGHT_BLUE, height=60)
		bottom_pattern.pack(side=tk.BOTTOM, fill=tk.X)

	def on_show(self):
		self.row_difficulty.refresh()
		self.row_pits.refresh()
		self.row_wumpus.refresh()
		self.focus_set()


def main():
	app = App()
	app.mainloop()


if __name__ == "__main__":
	main()
