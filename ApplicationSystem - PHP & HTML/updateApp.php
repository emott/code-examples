<!DOCTYPE html>
<html>
<head>
	<title>Update Application</title>
</head>
<body>
<?php
	session_start();

	if(isset($_POST['return'])){
		header("Location: main.html");
	}

	function populateYear($year){
		echo "<b>Year:</b><input type=\"radio\" name=\"year\" value=\"10\" required "; 
		if($year === '10'){ echo "checked";}
		echo ">10";
		echo "<input type=\"radio\" name=\"year\" value=\"11\" required "; 
		if($year === '11'){ echo "checked";}
		echo ">11";
		echo "<input type=\"radio\" name=\"year\" value=\"12\" required "; 
		if($year === '12'){ echo "checked";}
		echo ">12<br><br>";
	}

	function populateGender($gender){
		echo "<b>Gender</b><input type=\"radio\" name=\"gender\" value=\"M\" required ";
		if($gender === 'M'){ echo "checked";}
		echo ">M";
		echo "<input type=\"radio\" name=\"gender\" value=\"F\" required ";
		if($gender === 'F'){ echo "checked";}
		echo ">F<br><br>";
	}

	$email = $_SESSION['email'];
	$password = $_SESSION['password'];

	$db_connection = new mysqli("localhost", "dbuser", "goodbyeWorld", "applicationdb");
	if ($db_connection->connect_error) {
		die($db_connection->connect_error);
	}

	// NEED TO ENCRYPT PASSWORD. POSSIBLY DONE IN LECTURE COMING WEEK?
	$query = <<<QUERY
	SELECT name, email, gpa, year, gender, password FROM applicants WHERE email="$email" AND password="$password";
QUERY;
	
	$result = $db_connection->query($query);
	if(!$result){
		die("Retrieval failed: ". $db_connection->error);
	} else {
		if ( $result->num_rows === 0) { //no results
			$_SESSION['loginMessage'] = "Invalid email and/or password";
			header("Location: login.php");
		} else {	
			$result->data_seek(0);
			$data = $result->fetch_array(MYSQLI_ASSOC);
			
			echo <<<BODY
			<form action="confirmationUpdate.php" method="post">
				<b>Name:</b><input type="text" name="name" value="{$data['name']}" required><br><br>
				<b>Email:</b><input type="email" name="email" value="{$data['email']}" required><br><br>
				<b>GPA:</b><input type="number" name="gpa" min="0.0" max="4.0" step=".1" value="{$data['gpa']}" required><br><br>
BODY;

				populateYear($data['year']);
				populateGender($data['gender']);
				
				echo <<<BODY
				<b>Password:</b><input type="password" name="password" required><br><br>
				<b>Verify Password:</b><input type="password" name="verifyPass" required><br><br>

				<input type="submit" name="submit" value="Submit Data"><br><br>
			
			</form>

			<form action="submitApp.php" method="post">
				<input type="submit" name="return" value="Return to main menu">
			</form>
BODY;
	

		}
	}
?>
</body>
</html>