import tkinter as tk
from tkinter import ttk, messagebox
import os
from PIL import Image, ImageTk

# Import the WumpusGUI from wumpus_gui
try:
    from wumpus_gui import WumpusGUI
    WUMPUS_GUI_AVAILABLE = True
except ImportError as e:
    WUMPUS_GUI_AVAILABLE = False
    print(f"WumpusGUI not available: {e}")


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
		self.title("Treasure Hunt")
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

		self.container = tk.Frame(self, bg=BG_LIGHT_BLUE)
		self.container.pack(fill=tk.BOTH, expand=True)

		self.frames = {}
		for F in (MainMenuFrame,):
			frame = F(parent=self.container, controller=self)
			self.frames[F.__name__] = frame
			frame.place(relx=0, rely=0, relwidth=1, relheight=1)

		self.show_frame("MainMenuFrame")

	def show_frame(self, name: str) -> None:
		self.frames[name].tkraise()
		if hasattr(self.frames[name], "on_show"):
			self.frames[name].on_show()

	def launch_wumpus_game(self) -> None:
		"""Launch the WumpusGUI game and close launcher (single mainloop)"""
		if not WUMPUS_GUI_AVAILABLE:
			messagebox.showerror("Error", "WumpusGUI is not available. Please check the installation.")
			return
		
		try:
			# Destroy the launcher window completely (single mainloop approach)
			self.destroy()
			
			# Create and run the game with its own mainloop
			game_window = WumpusGUI()
			game_window.mainloop()
			
		except Exception as e:
			messagebox.showerror("Error", f"Failed to launch Wumpus game: {e}")
			print(f"Error launching WumpusGUI: {e}")


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
			command=controller.launch_wumpus_game,
			width=220,
			height=55
		)
		start_btn.pack(pady=20)

		# Bottom area with light blue background
		bottom_pattern = tk.Frame(self, bg=BG_LIGHT_BLUE, height=60)
		bottom_pattern.pack(side=tk.BOTTOM, fill=tk.X)

	def on_show(self):
		self.focus_set()


def main():
	app = App()
	app.mainloop()


if __name__ == "__main__":
	main()
