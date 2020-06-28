# chlorophyt.us-api
The API for chlorophyt.us
## Creating the MySQL database
### Schema
Simple as this:
```sql
CREATE DATABASE `ChlorophytusSchema`;
```
### Portfolio
```sql
CREATE TABLE `FolioTableEntries` (
  `id` bigint(20) unsigned zerofill NOT NULL AUTO_INCREMENT,
  `title` tinyblob NOT NULL,
  `description` blob NOT NULL,
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `link` tinyblob NOT NULL,
  `image` tinyblob NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_UNIQUE` (`id`)
);
```
### Blog
```sql
CREATE TABLE `BlogTableEntries` (
  `id` bigint(20) unsigned zerofill NOT NULL AUTO_INCREMENT,
  `title` tinyblob NOT NULL,
  `description` blob NOT NULL,
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_UNIQUE` (`id`)
);
```
### MOTD
```sql
CREATE TABLE `MOTDTableEntries` (
  `id` bigint(20) unsigned zerofill NOT NULL AUTO_INCREMENT,
  `title` tinyblob NOT NULL,
  `description` blob NOT NULL,
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_UNIQUE` (`id`)
);
```