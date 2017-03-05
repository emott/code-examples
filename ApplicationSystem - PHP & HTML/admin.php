<!DOCTYPE html>
<html>
<head>
	<title>Applications</title>
</head>
<body>
	<h1>Applications</h1>

	<?php
		session_start();
		
		if(isset($_POST['return'])){
			header("Location: main.html");
		}

	?>


	<form action="adminDisplay.php" method="post">
		<b>Select fields to display</b><br>
		<select name="fields[]" multiple size="5">
			<option value="name">name</option>
			<option value="email">email</option>
			<option value="gpa">gpa</option>
			<option value="year">year</option>
			<option value="gender">gender</option>
		</select><br><br>

		<b>Select field to sort applications</b>
		<select name="sort">
			<option value="name">name</option>
			<option value="email">email</option>
			<option value="gpa">gpa</option>
			<option value="year">year</option>
			<option value="gender">gender</option>
		</select><br><br>

		<b>Filter Condition</b>
		<input type="text" name="condition"><br><br>
		<input type="submit" name="display" value="Display Applications"><br><br>
	</form>
		
	<form action="admin.php" method="post">	
		<input type="submit" name="return" value="Return to main menu">
	</form>
	
</body>
</html>