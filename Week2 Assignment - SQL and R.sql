-- Create DB

CREATE DATABASE `moviereviews` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */;

USE moviereviews;

-- Create tables to store data: Movies and Reviewers

create table movies(
MovieId Int Primary Key,
Title Varchar(100) Not null
);

create table reviewers(
RevId int Primary Key,
Revname Varchar(100),
Rating int Not null,
MovieId int Null References movies
);

-- Populate tables

insert into movies values(10, "Black Panther");
insert into movies values(20, "A Wrinkle in Time");
insert into movies values(30, "Tom Rider");
insert into movies values(40, "Isle of Dogs");
insert into movies values(50, "Avengers: Infinity War");
insert into movies values(60, "Incredibles 2");

insert into reviewers values(1, "Mom", 3, 10);
insert into reviewers values(2, "Mom", 5, 20);
insert into reviewers values(3, "Mom", 2, 30);
insert into reviewers values(4, "Mom", 1, 40);
insert into reviewers values(5, "Mom", 4, 50);
insert into reviewers values(6, "Mom", 5, 60);

insert into reviewers values(11, "Daughter", 2, 10);
insert into reviewers values(22, "Daughter", 5, 20);
insert into reviewers values(33, "Daughter", 1, 30);
insert into reviewers values(44, "Daughter", 5, 40);
insert into reviewers values(55, "Daughter", 2, 50);
insert into reviewers values(66, "Daughter", 5, 60);

insert into reviewers values(111, "Friend1", 5, 10);
insert into reviewers values(222, "Friend1", 1, 20);
insert into reviewers values(333, "Friend1", 5, 30);
insert into reviewers values(444, "Friend1", 3, 40);
insert into reviewers values(555, "Friend1", 5, 50);
insert into reviewers values(666, "Friend1", 3, 60);

insert into reviewers values(1111, "Friend2", 1, 10);
insert into reviewers values(2222, "Friend2", 4, 20);
insert into reviewers values(3333, "Friend2", 2, 30);
insert into reviewers values(4444, "Friend2", 5, 40);
insert into reviewers values(5555, "Friend2", 2, 50);
insert into reviewers values(6666, "Friend2", 4, 60);

insert into reviewers values(11111, "Wife", 1, 10);
insert into reviewers values(22222, "Wife", 5, 20);
insert into reviewers values(33333, "Wife", 1, 30);
insert into reviewers values(44444, "Wife", 3, 40);
insert into reviewers values(55555, "Wife", 1, 50);
insert into reviewers values(66666, "Wife", 5, 60);


select * from movies;
select * from reviewers;

-- Report on Movie Reviews

select M.movieId, M.title, R.revId, R.revname, R.rating
from movies M
inner join reviewers R
on M.movieId = R.movieId

-- Little fix for R to be able to connect
-- ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'Pass@word1';
